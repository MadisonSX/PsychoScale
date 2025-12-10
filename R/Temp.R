
# Ipicq-ir ----

# 提取目标变量+转为数值型
target_vars <- paste0("cat_", p, c(2,7,9))

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

final_row_means[["Ipicq-ir"]] <- rowMeans(all_row_sums)

# Ipicq-na ----

# 提取目标变量+转为数值型
target_vars <- paste0("cat_", p, c(10,12,13))
imputed_data <- imputed_data_maxit5_ipicq

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

final_row_means[["Ipicq-na"]] <- rowMeans(all_row_sums)

# Ipicq-d ----

# 提取目标变量+转为数值型
target_vars <- paste0("cat_", p, c(3,5,8))
imputed_data <- imputed_data_maxit5_ipicq

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

final_row_means[["Ipicq-d"]] <- rowMeans(all_row_sums)

# Ipicq-es ----

# 提取目标变量+转为数值型
target_vars <- paste0("cat_", p, c(4,11,14))
imputed_data <- imputed_data_maxit5_ipicq

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

final_row_means[["Ipicq-es"]] <- rowMeans(all_row_sums)

# Ipicq-cm ----

# 提取目标变量+转为数值型
target_vars <- paste0("cat_", p, c(1,6,15))
imputed_data <- imputed_data_maxit5_ipicq

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

final_row_means[["Ipicq-cm"]] <- rowMeans(all_row_sums)
