# 脚本名称: 04_MASK总分.R
# 功能: 导入MASK数据并多重插补，按答案键计分，计算各得分类别总分均值。
# 输入: data/raw/EpiData_MASK.xls
#       data/raw/key.xlsx
# 输出: data/processed/EpiData_MASK.rds (原始数据备份)
#       data/processed/EpiData_MASK_final.xlsx

# 导入excel ----

suppressMessages(EpiData_MASK <- read_excel(here("data", "raw", "EpiData_MASK.xls")))
saveRDS(EpiData_MASK, here("data", "processed", "EpiData_MASK.rds")) # 保存原始数据

# 多重插补 ----

Data <- EpiData_MASK # 创建新的数据框
Data$GRADE <- factor(Data$GRADE,
                     levels = c("初一", "初二", "高一", "高二"),
                     labels = c(1, 2, 3, 4)) %>% as.numeric()
Data <- rename_categorical_vars(Data) # 对Data进行自动识别和重命名

# 因子化
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

# 查看变量类型
var_types <- sapply(Data, class)
# print(var_types)

# 分块插补（假设变量分为block1和block2）
# blocks <- list(block1 = c("var1", "var2"), block2 = c("var3", "var4"))
# imp <- mice(data, blocks = blocks, method = "rf", maxit = 3, m = 5, seed = 123)

# 设置插补方法
methods <- rep("pmm", ncol(Data))  # 默认用PMM（预测均值匹配）
names(methods) <- colnames(Data)
methods[grep("^bin_", names(methods))] <- "logreg"   # 二分类变量
methods[grep("^cat_", names(methods))] <- "polr"  # 有序分类变量

# 执行多重插补
imputed_data <- mice(
  Data,
  m = 5,
  method = methods, # 或更换为"rf"
  maxit = 5,
  seed = 123,
  nnet.MaxNWts = 5000,
  printFlag = TRUE  # 显示进度
)

# 求和 ----

# 将插补后的数据转换为数据框列表
imputed_data_list <- complete(imputed_data, action = "all")
suppressMessages(key <- read_excel(here("data", "raw", "key.xlsx")))
for(j in 1:5){
  test_data <- imputed_data_list[[j]]
  mask_cols <- paste0("cat_MASK", 1:45)
  
  for(i in seq_along(mask_cols)) {
    col_name <- mask_cols[i]
    mapping <- unlist(key[i, ])
    test_data[[col_name]] <- mapping[as.character(test_data[[col_name]])]
  }
  test_data$正确 <- rowSums(test_data[mask_cols] == "正确")
  test_data$过度 <- rowSums(test_data[mask_cols] == "过度")
  test_data$不足 <- rowSums(test_data[mask_cols] == "不足")
  test_data$无 <- rowSums(test_data[mask_cols] == "无")
  
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

final_row_means <- Data %>%
  select(1:5)
vec <- c("正确", "过度", "不足", "无", "控制问题")
for(p in vec){
  all_row_sums <- do.call(cbind, lapply(imputed_data_list, `[[`, p))
  final_row_means[[p]] <- rowMeans(all_row_sums)
}

write_xlsx(
  final_row_means,          # 要导出的数据框
  here("data", "processed", "EpiData_MASK_final.xlsx"),    # 输出路径
)
