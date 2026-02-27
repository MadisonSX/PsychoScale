# ==============================================================================
# 脚本名称: 03_FACE总分.R
# 功能: 导入FACE数据并多重插补，按答案键计分，计算表情与性别得分均值。
# 输入: data/raw/RawData_FACE.xls
#       data/raw/key_FACE.xlsx
# 输出: data/processed/final_row_means_rounded_FACE.xlsx
# ==============================================================================

# 安装并加载所需包 ----
required_packages <- c("readxl", "dplyr", "here", "mice", "writexl")
installed_packages <- rownames(installed.packages())
for (pkg in required_packages) {
  if (!pkg %in% installed_packages) {
    install.packages(pkg)
  }
  suppressPackageStartupMessages(library(pkg, character.only = TRUE))
}

# 1. 导入数据 =====================================================================

suppressMessages(EpiData_FACE <- read_excel(here("data", "raw", "RawData_FACE.xls")))

# 2. 数据预处理 =====================================================================

# 创建工作副本
Data <- EpiData_FACE

# 转换年级变量
Data$GRADE <- factor(Data$GRADE,
                     levels = c("初一", "初二", "高一", "高二"),
                     labels = c(1, 2, 3, 4)) %>% as.numeric()

# 自动识别和重命名变量
Data <- rename_categorical_vars(Data)

# 因子化处理
Data <- Data %>%
  mutate(
    # 二分类变量
    across(starts_with("bin_"), as.factor),
    # 有序分类变量
    across(starts_with("cat_"), ~ {
      unique_vals <- sort(unique(na.omit(.x)))
      factor(.x, levels = unique_vals, ordered = TRUE)
    })
  )

# 3. 多重插补 =====================================================================

# 设置插补方法
methods <- rep("pmm", ncol(Data))
names(methods) <- colnames(Data)
methods[grep("^bin_", names(methods))] <- "logreg"    # 二分类变量
methods[grep("^cat_", names(methods))] <- "polr"      # 有序分类变量

# 执行多重插补
imputed_data <- mice(
  Data,
  m = 5,
  method = "cart",
  maxit = 5,
  seed = 123,
  nnet.MaxNWts = 5000,
  printFlag = TRUE
)

# 4. 计分 =====================================================================

# 获取所有插补数据集
imputed_data_list <- complete(imputed_data, action = "all")
suppressMessages(key <- read_excel(here("data", "raw", "key_FACE.xlsx")))
correct_answers <- setNames(as.list(key$key), key$colnames)

# 对每个插补数据集进行计分
for (j in 1:5) {
  test_data <- imputed_data_list[[j]]
  names(test_data) <- gsub("cat_|bin_", "", names(test_data))
  
  # 表情得分
  cols <- names(correct_answers)[grepl("A$", names(correct_answers))]
  correct_matrix <- sapply(cols, function(col) {
    test_data[[col]] == correct_answers[[col]]
  })
  test_data$表情 <- rowSums(correct_matrix, na.rm = TRUE)
  
  # 性别得分
  cols <- names(correct_answers)[grepl("B$", names(correct_answers))]
  correct_matrix <- sapply(cols, function(col) {
    test_data[[col]] == correct_answers[[col]]
  })
  test_data$性别 <- rowSums(correct_matrix, na.rm = TRUE)
  
  imputed_data_list[[j]] <- test_data
}

# 5. 汇总结果 =====================================================================

# 计算各肌分估计值（5个插补数据集的均值）
final_row_means <- Data %>%
  select(1:5)

vec <- c("表情", "性别")
for (p in vec) {
  all_row_sums <- do.call(cbind, lapply(imputed_data_list, `[[`, p))
  final_row_means[[p]] <- rowMeans(all_row_sums)
}

# 四舍五入处理
final_row_means_rounded <- final_row_means %>%
  mutate(across(where(is.numeric), ~ round(., digits = 0)))

# 6. 结果导出 =====================================================================

write_xlsx(
  final_row_means_rounded,
  here("data", "processed", "final_row_means_rounded_FACE.xlsx")
)

