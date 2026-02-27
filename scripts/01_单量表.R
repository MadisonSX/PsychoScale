# 脚本名称: 01_单量表.R
# 功能: 导入单量表数据，按变量类型设置插补方法并进行多重插补，
#       计算量表题目行和并输出均值总分。
# 输入: data/raw/RawData_emophq.xlsx
# 输出: data/processed/RawData_emophq.rds (原始数据备份)
#       对象 final_row_means (量表总分均值)
# 作者: Yujie
# 更新日期: 2025-04-12

source(here::here("R", "functions.R"))

# 导入excel ----

suppressMessages(RawData_emophq <- read_excel(here("data", "raw", "RawData_emophq.xlsx")))
saveRDS(RawData_emophq, here("data", "processed", "RawData_emophq.rds")) # 保存原始数据

# 多重插补 ----

Data <- RawData_emophq # 创建新的数据框
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

# 验证插补结果 ----

# 敏感性分析

# 模型收敛性诊断
plot(imputed_data, c("cat_emophq1", "cat_emophq4"))
stripplot(imputed_data, pch=19, cex=1.2, alpha=.3)

# 插补值分布合理性评估

# 统计指标验证

# 求和 ----

# 提取目标变量+转为数值型
target_vars <- grep("emophq", names(Data), value = TRUE)
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
final_row_means <- rowMeans(all_row_sums)
