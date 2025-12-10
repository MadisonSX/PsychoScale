# 导入excel ----

suppressMessages(EpiData_FACE <- read_excel(here("data", "raw", "EpiData_FACE.xls")))
saveRDS(EpiData_FACE, here("data", "processed", "EpiData_FACE.rds")) # 保存原始数据

# 多重插补 ----

Data <- EpiData_FACE # 创建新的数据框
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
  method = "cart", # 或更换为"rf", methods, "cart"
  maxit = 5,
  seed = 123,
  nnet.MaxNWts = 5000,
  printFlag = TRUE  # 显示进度
)

# 求和 ----

# 将插补后的数据转换为数据框列表
imputed_data_list <- complete(imputed_data, action = "all")
suppressMessages(key <- read_excel(here("data", "raw", "key_face.xlsx")))
correct_answers <- setNames(as.list(key$key), key$colnames)
for(j in 1:5){
  test_data <- imputed_data_list[[j]]
  names(test_data) <- gsub("cat_|bin_", "", names(test_data))
  
  cols <- names(correct_answers)[grepl("A$", names(correct_answers))]
  correct_matrix <- sapply(cols, function(col) {
    test_data[[col]] == correct_answers[[col]]
  })
  test_data$表情 <- rowSums(correct_matrix, na.rm = TRUE)
  
  cols <- names(correct_answers)[grepl("B$", names(correct_answers))]
  correct_matrix <- sapply(cols, function(col) {
    test_data[[col]] == correct_answers[[col]]
  })
  test_data$性别 <- rowSums(correct_matrix, na.rm = TRUE)
  
  imputed_data_list[[j]] <- test_data
}

final_row_means <- Data %>%
  select(1:5)
vec <- c("表情", "性别")
for(p in vec){
  all_row_sums <- do.call(cbind, lapply(imputed_data_list, `[[`, p))
  final_row_means[[p]] <- rowMeans(all_row_sums)
}

final_row_means_rounded <- final_row_means %>%
  mutate(across(where(is.numeric), ~ round(., digits = 0)))

write_xlsx(
  final_row_means_rounded,          # 要导出的数据框
  here("data", "processed", "EpiData_FACE_final.xlsx"),    # 输出路径
)

# 备份 ----
backup_workspace()
