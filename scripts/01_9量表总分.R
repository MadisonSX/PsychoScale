# 脚本名称: 01_9量表总分.R
# 功能: 导入多量表数据，按量表拆分并多重插补，计算各量表总分均值及分量表分数，
#       生成分量表分数并导出四舍五入结果。
# 输入: data/raw/RawData.xlsx
# 输出: data/processed/final_row_means_rounded.xlsx
# 作者: Yujie
# 更新日期: 2025-04-12

# 加载或安装包 ----
packages <- c("here", "readxl", "dplyr", "mice", "openxlsx")

for (pkg in packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}

source(here("R", "functions.R"))

# 导入excel ----

suppressMessages(RawData <- read_excel(here("data", "raw", "RawData.xlsx")))

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



# 分量表求和 ====

# emophq系列只计分前9题 ----

# 提取目标变量+转为数值型
p <- "emophq"
imputed_data <- imputed_data_maxit5_emophq
target_vars <- paste0("cat_", p, 1:9)

imputed_list <- lapply(1:imputed_data$m, function(i) {
  complete(imputed_data, i)[, target_vars]
})
imputed_list <- lapply(imputed_list, function(df) {
  as.data.frame(apply(df, 2, as.numeric))  # 将所有列强制转换为数值型[6,8]
})

# 计算行和并合并
imputed_list_with_sum <- lapply(imputed_list, function(df) {
  df$row_sum <- rowSums(df, na.rm = TRUE)
  return(df)
})
all_row_sums <- do.call(cbind, lapply(imputed_list_with_sum, `[[`, "row_sum"))

final_row_means[[p]] <- rowMeans(all_row_sums)

# ipicq+因子 ----

p <- "ipicq"
imputed_data <- imputed_data_maxit5_ipicq
list <- list(c(2,7,9), c(10,12,13), c(3,5,8), c(4,11,14), c(1,6,15))
ipicq <- c("ir","na","d","es","cm")

for(i in 1:5){
  # 提取目标变量+转为数值型
  target_vars <- paste0("cat_", p, list[[i]])
  
  imputed_list <- lapply(1:imputed_data$m, function(i) {
    complete(imputed_data, i)[, target_vars]
  })
  imputed_list <- lapply(imputed_list, function(df) {
    as.data.frame(apply(df, 2, as.numeric))  # 将所有列强制转换为数值型[6,8]
  })
  
  # 计算行和并合并
  imputed_list_with_sum <- lapply(imputed_list, function(df) {
    df$row_sum <- rowSums(df, na.rm = TRUE)
    return(df)
  })
  all_row_sums <- do.call(cbind, lapply(imputed_list_with_sum, `[[`, "row_sum"))
  
  final_row_means[[paste0(p,"-",ipicq[i])]] <- rowMeans(all_row_sums)
}

# ipiip仅因子 ----

p <- "ipiip"
imputed_data <- imputed_data_maxit5_ipiip
list <- list(c(1,9,17,25), c(2,10,18,26), c(3,11,19,27), c(4,12,20,28), c(5,13,21,29), c(6,14,22,30), c(7,15,23,31), c(8,16,24,32))
ipiip <- c("zd","jz","lj","tb","sc","jn","xs","bd")

for(i in 1:8){
  # 提取目标变量+转为数值型
  target_vars <- paste0("cat_", p, list[[i]])
  
  imputed_list <- lapply(1:imputed_data$m, function(i) {
    complete(imputed_data, i)[, target_vars]
  })
  imputed_list <- lapply(imputed_list, function(df) {
    as.data.frame(apply(df, 2, as.numeric))  # 将所有列强制转换为数值型[6,8]
  })
  
  # 计算行和并合并
  imputed_list_with_sum <- lapply(imputed_list, function(df) {
    df$row_sum <- rowSums(df, na.rm = TRUE)
    return(df)
  })
  all_row_sums <- do.call(cbind, lapply(imputed_list_with_sum, `[[`, "row_sum"))
  
  final_row_means[[paste0(p,"-",ipiip[i])]] <- rowMeans(all_row_sums)
}

# erirq仅因子 ----

p <- "erirq"
imputed_data <- imputed_data_maxit5_erirq
list <- list(1:4, 5:8, 9:12, 13:16)
erirq <- c("xq","xx","jq","jx")

for(i in 1:4){
  # 提取目标变量+转为数值型
  target_vars <- paste0("cat_", p, list[[i]])
  
  imputed_list <- lapply(1:imputed_data$m, function(i) {
    complete(imputed_data, i)[, target_vars]
  })
  imputed_list <- lapply(imputed_list, function(df) {
    as.data.frame(apply(df, 2, as.numeric))  # 将所有列强制转换为数值型[6,8]
  })
  
  # 计算行和并合并
  imputed_list_with_sum <- lapply(imputed_list, function(df) {
    df$row_sum <- rowSums(df, na.rm = TRUE)
    return(df)
  })
  all_row_sums <- do.call(cbind, lapply(imputed_list_with_sum, `[[`, "row_sum"))
  
  final_row_means[[paste0(p,"-",erirq[i])]] <- rowMeans(all_row_sums)
}

# ererq仅因子 ----

p <- "ererq"
imputed_data <- imputed_data_maxit5_ererq
list <- list(c(1,3,5,7,8,10), c(2,4,6,9))
ererq <- c("cp","yz")

for(i in 1:2){
  # 提取目标变量+转为数值型
  target_vars <- paste0("cat_", p, list[[i]])
  
  imputed_list <- lapply(1:imputed_data$m, function(i) {
    complete(imputed_data, i)[, target_vars]
  })
  imputed_list <- lapply(imputed_list, function(df) {
    as.data.frame(apply(df, 2, as.numeric))  # 将所有列强制转换为数值型[6,8]
  })
  
  # 计算行和并合并
  imputed_list_with_sum <- lapply(imputed_list, function(df) {
    df$row_sum <- rowSums(df, na.rm = TRUE)
    return(df)
  })
  all_row_sums <- do.call(cbind, lapply(imputed_list_with_sum, `[[`, "row_sum"))
  
  final_row_means[[paste0(p,"-",ererq[i])]] <- rowMeans(all_row_sums)
}

# erders+因子 ----

p <- "erders"
imputed_data <- imputed_data_maxit5_erders
list <- list(c(1,2), c(3,7,15), c(4,8,11), c(5,6,12,14,16), c(9,10,13))
erders <- c("qxx","dxtr","cd","cl","bjs")

for(i in 1:5){
  # 提取目标变量+转为数值型
  target_vars <- paste0("cat_", p, list[[i]])
  
  imputed_list <- lapply(1:imputed_data$m, function(i) {
    complete(imputed_data, i)[, target_vars]
  })
  imputed_list <- lapply(imputed_list, function(df) {
    as.data.frame(apply(df, 2, as.numeric))  # 将所有列强制转换为数值型[6,8]
  })
  
  # 计算行和并合并
  imputed_list_with_sum <- lapply(imputed_list, function(df) {
    df$row_sum <- rowSums(df, na.rm = TRUE)
    return(df)
  })
  all_row_sums <- do.call(cbind, lapply(imputed_list_with_sum, `[[`, "row_sum"))
  
  final_row_means[[paste0(p,"-",erders[i])]] <- rowMeans(all_row_sums)
}

# 四舍五入并导出 ----

final_row_means_rounded <- final_row_means %>%
  mutate(across(where(is.numeric), ~ round(., digits = 0)))

write.xlsx(
  final_row_means_rounded,
  here("data", "processed", "final_row_means_rounded.xlsx")
)
