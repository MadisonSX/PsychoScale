# ==============================================================================
# 脚本名称: 01_9量表总分.R
# 功能: 导入多量表数据，按量表拆分并多重插补，计算各量表总分均值及分量表分数，
#       生成分量表分数并导出四舍五入结果。
# 输入: 1st_time/input/RawData_multiple.xlsx
# 输出: 1st_time/output/final_row_means_rounded_multiple.xlsx
# 作者: Yujie
# 更新日期: 2025-04-12
# ==============================================================================

# 1. 包管理 =====================================================================

packages <- c("here", "readxl", "dplyr", "mice", "openxlsx")

for (pkg in packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}

source(here("R", "functions.R"))

# 2. 数据导入与基础统计 ===========================================================

suppressMessages(RawData <- read_excel(here("1st_time", "input", "RawData_multiple.xlsx")))

# 计算缺失率情况
col_miss <- colMeans(is.na(RawData)) * 100
sort(col_miss[col_miss > 0 & col_miss < 5], decreasing = TRUE)
summary(col_miss)

row_miss <- rowMeans(is.na(RawData)) * 100
summary(row_miss)

# 3. 多重插补处理 =================================================================

# 初始化结果数据框
final_row_means <- RawData %>%
  select(1:4)

# 提取量表列名模式（去掉末尾数字）
Rawcolnames <- colnames(RawData)
colnames <- grep("(?<![0-9])1$", Rawcolnames, value = TRUE, perl = TRUE)
colnames <- sub("1$", "", colnames)
patterns <- colnames

# 对每个量表进行多重插补处理
for (p in patterns) {
  df_name <- paste0("RawData_", p)
  target_cols <- grep(p, colnames(RawData), value = TRUE)
  
  if (length(target_cols) > 0) {
    # 提取该量表数据
    Data <- RawData %>%
      select(1:3, all_of(target_cols))
    
    # 自动识别和重命名分类变量
    Data <- rename_categorical_vars(Data)
    
    # 因子化变量处理
    Data <- Data %>%
      mutate(
        number = as.character(number),
        across(starts_with("bin_"), as.factor),
        across(starts_with("cat_"), ~ {
          unique_vals <- unique(na.omit(.x))
          numeric_vals <- as.numeric(unique_vals)
          min_val <- min(numeric_vals)
          max_val <- max(numeric_vals)
          full_levels <- seq(min_val, max_val)
          factor(.x, levels = full_levels, ordered = TRUE)
        })
      )
    
    # 设置插补方法：PMM用于连续变量，logreg用于二分类，polr用于有序分类
    methods <- rep("pmm", ncol(Data))
    names(methods) <- colnames(Data)
    methods[grep("^bin_", names(methods))] <- "logreg"
    methods[grep("^cat_", names(methods))] <- "polr"
    
    # 执行多重插补
    maxit <- 5
    imputed_data <- mice(
      Data,
      m = 5,
      method = methods,
      maxit = maxit,
      seed = 123,
      nnet.MaxNWts = 5000,
      printFlag = TRUE
    )
    
    # 保存插补结果对象
    mice_name <- paste0("imputed_data_maxit", maxit, "_", p)
    assign(mice_name, imputed_data)
    
    # 计算该量表的总分
    target_vars <- grep(p, names(Data), value = TRUE)
    imputed_list <- lapply(1:imputed_data$m, function(i) {
      complete(imputed_data, i)[, target_vars]
    })
    imputed_list <- lapply(imputed_list, function(df) {
      as.data.frame(apply(df, 2, as.numeric))
    })
    
    # 计算行和的均值
    imputed_list_with_sum <- lapply(imputed_list, function(df) {
      df$row_sum <- rowSums(df, na.rm = TRUE)
      return(df)
    })
    all_row_sums <- do.call(cbind, lapply(imputed_list_with_sum, `[[`, "row_sum"))
    final_row_means[[p]] <- rowMeans(all_row_sums)
    
  } else {
    warning(paste("未找到含", p, "的列，跳过该模式"))
    next
  }
}

# 4. 分量表求和处理 ================================================================

# 4.1 emophq系列 - 只计分前9题
p <- "emophq"
imputed_data <- imputed_data_maxit5_emophq
target_vars <- paste0("cat_", p, 1:9)

imputed_list <- lapply(1:imputed_data$m, function(i) {
  complete(imputed_data, i)[, target_vars]
})
imputed_list <- lapply(imputed_list, function(df) {
  as.data.frame(apply(df, 2, as.numeric))
})

imputed_list_with_sum <- lapply(imputed_list, function(df) {
  df$row_sum <- rowSums(df, na.rm = TRUE)
  return(df)
})
all_row_sums <- do.call(cbind, lapply(imputed_list_with_sum, `[[`, "row_sum"))
final_row_means[[p]] <- rowMeans(all_row_sums)

# 4.2 ipicq - 5个因子
p <- "ipicq"
imputed_data <- imputed_data_maxit5_ipicq
factor_items <- list(c(2,7,9), c(10,12,13), c(3,5,8), c(4,11,14), c(1,6,15))
factor_names <- c("ir","na","d","es","cm")

for(i in 1:5){
  target_vars <- paste0("cat_", p, factor_items[[i]])
  imputed_list <- lapply(1:imputed_data$m, function(j) {
    complete(imputed_data, j)[, target_vars]
  })
  imputed_list <- lapply(imputed_list, function(df) {
    as.data.frame(apply(df, 2, as.numeric))
  })
  
  imputed_list_with_sum <- lapply(imputed_list, function(df) {
    df$row_sum <- rowSums(df, na.rm = TRUE)
    return(df)
  })
  all_row_sums <- do.call(cbind, lapply(imputed_list_with_sum, `[[`, "row_sum"))
  final_row_means[[paste0(p,"-",factor_names[i])]] <- rowMeans(all_row_sums)
}

# 4.3 ipiip - 8个因子
p <- "ipiip"
imputed_data <- imputed_data_maxit5_ipiip
factor_items <- list(c(1,9,17,25), c(2,10,18,26), c(3,11,19,27), c(4,12,20,28), 
                      c(5,13,21,29), c(6,14,22,30), c(7,15,23,31), c(8,16,24,32))
factor_names <- c("zd","jz","lj","tb","sc","jn","xs","bd")

for(i in 1:8){
  target_vars <- paste0("cat_", p, factor_items[[i]])
  imputed_list <- lapply(1:imputed_data$m, function(j) {
    complete(imputed_data, j)[, target_vars]
  })
  imputed_list <- lapply(imputed_list, function(df) {
    as.data.frame(apply(df, 2, as.numeric))
  })
  
  imputed_list_with_sum <- lapply(imputed_list, function(df) {
    df$row_sum <- rowSums(df, na.rm = TRUE)
    return(df)
  })
  all_row_sums <- do.call(cbind, lapply(imputed_list_with_sum, `[[`, "row_sum"))
  final_row_means[[paste0(p,"-",factor_names[i])]] <- rowMeans(all_row_sums)
}

# 4.4 erirq - 4个因子
p <- "erirq"
imputed_data <- imputed_data_maxit5_erirq
factor_items <- list(1:4, 5:8, 9:12, 13:16)
factor_names <- c("xq","xx","jq","jx")

for(i in 1:4){
  target_vars <- paste0("cat_", p, factor_items[[i]])
  imputed_list <- lapply(1:imputed_data$m, function(j) {
    complete(imputed_data, j)[, target_vars]
  })
  imputed_list <- lapply(imputed_list, function(df) {
    as.data.frame(apply(df, 2, as.numeric))
  })
  
  imputed_list_with_sum <- lapply(imputed_list, function(df) {
    df$row_sum <- rowSums(df, na.rm = TRUE)
    return(df)
  })
  all_row_sums <- do.call(cbind, lapply(imputed_list_with_sum, `[[`, "row_sum"))
  final_row_means[[paste0(p,"-",factor_names[i])]] <- rowMeans(all_row_sums)
}

# 4.5 ererq - 2个因子
p <- "ererq"
imputed_data <- imputed_data_maxit5_ererq
factor_items <- list(c(1,3,5,7,8,10), c(2,4,6,9))
factor_names <- c("cp","yz")

for(i in 1:2){
  target_vars <- paste0("cat_", p, factor_items[[i]])
  imputed_list <- lapply(1:imputed_data$m, function(j) {
    complete(imputed_data, j)[, target_vars]
  })
  imputed_list <- lapply(imputed_list, function(df) {
    as.data.frame(apply(df, 2, as.numeric))
  })
  
  imputed_list_with_sum <- lapply(imputed_list, function(df) {
    df$row_sum <- rowSums(df, na.rm = TRUE)
    return(df)
  })
  all_row_sums <- do.call(cbind, lapply(imputed_list_with_sum, `[[`, "row_sum"))
  final_row_means[[paste0(p,"-",factor_names[i])]] <- rowMeans(all_row_sums)
}

# 4.6 erders - 5个因子
p <- "erders"
imputed_data <- imputed_data_maxit5_erders
factor_items <- list(c(1,2), c(3,7,15), c(4,8,11), c(5,6,12,14,16), c(9,10,13))
factor_names <- c("qxx","dxtr","cd","cl","bjs")

for(i in 1:5){
  target_vars <- paste0("cat_", p, factor_items[[i]])
  imputed_list <- lapply(1:imputed_data$m, function(j) {
    complete(imputed_data, j)[, target_vars]
  })
  imputed_list <- lapply(imputed_list, function(df) {
    as.data.frame(apply(df, 2, as.numeric))
  })
  
  imputed_list_with_sum <- lapply(imputed_list, function(df) {
    df$row_sum <- rowSums(df, na.rm = TRUE)
    return(df)
  })
  all_row_sums <- do.call(cbind, lapply(imputed_list_with_sum, `[[`, "row_sum"))
  final_row_means[[paste0(p,"-",factor_names[i])]] <- rowMeans(all_row_sums)
}

# 5. 结果四舍五入并导出 ===========================================================

# 四舍五入到整数
final_row_means_rounded <- final_row_means %>%
  mutate(across(where(is.numeric), ~ round(., digits = 0)))

# 导出结果到Excel
write.xlsx(
  final_row_means_rounded,
  here("1st_time", "output", "final_row_means_rounded_multiple.xlsx")
)

message("✓ 处理完成！结果已导出至 1st_time/output/final_row_means_rounded_multiple.xlsx")
