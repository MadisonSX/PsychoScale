# ============================================================================
# 脚本名称: 02_MASK总分.R
# 功能: 导入MASK数据并多重插补，按答案键计分，计算各得分类别总分均值。
# 输入: 
#   - data/raw/RawData_MASK.xls
#   - data/raw/key_MASK.xlsx
# 输出: 
#   - data/processed/final_row_means_rounded_MASK.xlsx
# ============================================================================

# 安装并加载所需包 ----
required_packages <- c("readxl", "dplyr", "here", "mice", "writexl")
installed_packages <- rownames(installed.packages())
for (pkg in required_packages) {
  if (!pkg %in% installed_packages) {
    install.packages(pkg)
  }
  suppressPackageStartupMessages(library(pkg, character.only = TRUE))
}

# 导入 Excel 数据 ----
suppressMessages(EpiData_MASK <- read_excel(here("data", "raw", "RawData_MASK.xls")))

# 数据预处理与多重插补 ----

# 创建工作副本并编码级别
Data <- EpiData_MASK
Data$GRADE <- factor(Data$GRADE,
                     levels = c("初一", "初二", "高一", "高二"),
                     labels = c(1, 2, 3, 4)) %>% as.numeric()
Data <- rename_categorical_vars(Data)

# 数据变量类型转换（因子化）
Data <- Data %>%
  mutate(
    across(starts_with("bin_"), as.factor),
    across(starts_with("cat_"), ~ {
      unique_vals <- sort(unique(na.omit(.x)))  # 获取非缺失唯一值并排序
      factor(.x, 
             levels = unique_vals, 
             ordered = TRUE)
    })
  )

# 配置多重插补参数
# 设置各变量的插补方法
methods <- rep("pmm", ncol(Data))
names(methods) <- colnames(Data)
methods[grep("^bin_", names(methods))] <- "logreg"     # 二分类变量
methods[grep("^cat_", names(methods))] <- "polr"       # 有序分类变量

# 执行多重插补 (m=5, maxit=5)
imputed_data <- mice(
  Data,
  m = 5,
  method = methods,
  maxit = 5,
  seed = 123,
  nnet.MaxNWts = 5000,
  printFlag = TRUE
)

# 按答案键计分与求和 ----

# 读取答案键
imputed_data_list <- complete(imputed_data, action = "all")
suppressMessages(key <- read_excel(here("data", "raw", "key_MASK.xlsx")))

# 对每个插补数据集进行计分
for(j in 1:5){
  test_data <- imputed_data_list[[j]]
  mask_cols <- paste0("cat_MASK", 1:45)
  
  for(i in seq_along(mask_cols)) {
    col_name <- mask_cols[i]
    mapping <- unlist(key[i, ])
    test_data[[col_name]] <- mapping[as.character(test_data[[col_name]])]
  }
  # 计算四个得分类别的总数
  test_data$正确 <- rowSums(test_data[mask_cols] == "正确")
  test_data$过度 <- rowSums(test_data[mask_cols] == "过度")
  test_data$不足 <- rowSums(test_data[mask_cols] == "不足")
  test_data$无 <- rowSums(test_data[mask_cols] == "无")
  
  # 计算控制问题正确率
  correct_answers <- list(
    "cat_MASK13C" = "c",
    "cat_MASK33C" = "a", 
    "cat_MASK38C" = "b",
    "cat_MASK46C" = "b",
    "cat_MASK47C" = "a",
    "cat_MASK48C" = "c"
  )
  correct_matrix <- sapply(names(correct_answers), function(col) {
    test_data[[col]] == correct_answers[[col]]
  })
  test_data$控制问题 <- rowSums(correct_matrix, na.rm = TRUE)
  
  imputed_data_list[[j]] <- test_data
}

# 计算多重插补数据的均值 ----
final_row_means <- Data %>%
  select(1:5)

# 对各得分类别求5个插补数据集的平均值
vec <- c("正确", "过度", "不足", "无", "控制问题")
for(p in vec){
  all_row_sums <- do.call(cbind, lapply(imputed_data_list, `[[`, p))
  final_row_means[[p]] <- rowMeans(all_row_sums)
}

# 导出结果 ----
write_xlsx(
  final_row_means,
  here("data", "processed", "final_row_means_rounded_MASK.xlsx")
)
