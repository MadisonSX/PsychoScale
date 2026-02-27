# 脚本名称: 02_多量表.R
# 功能: 导入多量表数据，按量表拆分并多重插补，计算各量表总分均值，
#       导出汇总表与各量表插补数据。
# 输入: data/raw/RawData.xlsx
# 输出: data/processed/RawData.rds (原始数据备份)
#       data/processed/final_row_means.csv
#       data/processed/final_row_means_rounded.csv
#       data/processed/Imputed_Data_maxit5_*.xlsx
#       data/processed/imputation_data_maxit5.RData
# 作者: Yujie
# 更新日期: 2025-04-12

source(here("R", "functions.R"))

# 导入excel ----

suppressMessages(RawData <- read_excel(here("data", "raw", "RawData.xlsx")))
saveRDS(RawData, here("data", "processed", "RawData.rds")) # 保存原始数据

# 计算缺失率
col_miss <- colMeans(is.na(RawData)) * 100
sort(col_miss[col_miss > 0 & col_miss < 5], decreasing = TRUE)
summary(col_miss)
row_miss <- rowMeans(is.na(RawData)) * 100
summary(row_miss)

# 多重插补 ----

final_row_means <- RawData %>%
  select(1:4)

Rawcolnames <- colnames(RawData)
colnames <- grep("(?<![0-9])1$", Rawcolnames, value = TRUE, perl = TRUE)
colnames <- sub("1$", "", colnames)
patterns <- colnames  # 定义需要匹配的多个字符串（可自行扩展）
for (p in patterns) {
  df_name <- paste0("RawData_", p)
  target_cols <- grep(p, colnames(RawData), value = TRUE)
  if (length(target_cols) > 0) {
    Data <- RawData %>%
      select(1:3, all_of(target_cols))
    # assign(df_name, Data)
    Data <- rename_categorical_vars(Data) # 对Data进行自动识别和重命名
    
    # 因子化
    Data <- Data %>%
      mutate(
        number = as.character(number),
        across(starts_with("bin_"), as.factor),
        across(starts_with("cat_"), ~ {
          unique_vals <- unique(na.omit(.x)) # 提取非缺失唯一值并转为数值
          numeric_vals <- as.numeric(unique_vals)
          min_val <- min(numeric_vals) # 获取最小值和最大值
          max_val <- max(numeric_vals)
          full_levels <- seq(min_val, max_val) # 生成从min到max的完整序列作为因子等级
          factor(.x, # 创建有序因子
                 levels = full_levels,
                 ordered = TRUE)
        })
      )
    
    # 设置插补方法
    methods <- rep("pmm", ncol(Data))  # 默认用PMM（预测均值匹配）
    names(methods) <- colnames(Data)
    methods[grep("^bin_", names(methods))] <- "logreg"   # 二分类变量
    methods[grep("^cat_", names(methods))] <- "polr"  # 有序分类变量
    
    # 执行多重插补
    maxit = 5
    imputed_data <- mice(
      Data,
      m = 5,
      method = methods, # 或更换为"rf"
      maxit = maxit,
      seed = 123,
      nnet.MaxNWts = 5000,
      printFlag = TRUE  # 显示进度
    )
    mice_name <- paste0("imputed_data_maxit", maxit, "_", p)
    assign(mice_name, imputed_data)
    
    # 验证插补结果 ----
    
    # 敏感性分析
    # 模型收敛性诊断
    # 插补值分布合理性评估
    # 统计指标验证
    
    # 求和 ----
    
    # 提取目标变量+转为数值型
    target_vars <- grep(p, names(Data), value = TRUE)
    imputed_list <- lapply(1:imputed_data$m, function(i) {
      complete(imputed_data, i)[, target_vars]
    })
    imputed_list <- lapply(imputed_list, function(df) {
      as.data.frame(apply(df, 2, as.numeric))  # 将所有列强制转换为数值型[6,8](@ref)
    })
    
    # 计算行和并合并
    imputed_list_with_sum <- lapply(imputed_list, function(df) {
      df$row_sum <- rowSums(df, na.rm = TRUE)
      return(df)
    })
    all_row_sums <- do.call(cbind, lapply(imputed_list_with_sum, `[[`, "row_sum"))
    
    # 输出最终行和的均值
    # col_name <- paste0("final_row_means_", p)
    # assign(col_name, rowMeans(all_row_sums))
    # final_row_means[[col_name]] <- rowMeans(all_row_sums)
    final_row_means[[p]] <- rowMeans(all_row_sums)
  } else {
    warning(paste("未找到含", p, "的列，跳过该模式"))
    next
  }
}

final_row_means_rounded <- final_row_means %>%
  mutate(across(where(is.numeric), ~ round(., digits = 0)))

write.csv(
  final_row_means,          # 要导出的数据框
  file = here("data", "processed", "final_row_means.csv"),    # 输出路径
)
write.csv(
  final_row_means_rounded,          # 要导出的数据框
  file = here("data", "processed", "final_row_means_rounded.csv"),    # 输出路径
)

# 导出插补数据表为xlsx
for (p in patterns) {
  imp_name <- paste0("imputed_data_maxit5_", p)
  if (!exists(imp_name)) {
    warning("对象 ", imp_name, " 不存在，已跳过")
    nexts
  }
  imp <- get(imp_name)
  wb <- createWorkbook()
  for (i in 1:imp$m) { # 将每个插补数据集写入不同 Sheet
    sheet_name <- paste("Imputation", i)  # Sheet 名称（如 "Imputation 1"）
    addWorksheet(wb, sheet_name)
    writeData(wb, sheet = sheet_name, x = complete(imp, action = i))
  }
  output_path <- here("data", "processed", paste0("Imputed_Data_maxit5_", p, ".xlsx"))
  saveWorkbook(wb, output_path, overwrite = TRUE)
  message("成功导出: ", output_path)
}

# 导出插补数据表及求和表为RData
dataset_names <- paste0("imputed_data_maxit5_", patterns)
existing_datasets <- dataset_names[sapply(dataset_names, exists)]
save(list = existing_datasets,final_row_means, file = here("data","processed","imputation_data_maxit5.RData"))

