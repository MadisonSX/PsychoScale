load(here("data","processed","imputation_data_maxit5.RData"))

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

# 四舍五入 ----
final_row_means_rounded <- final_row_means %>%
  mutate(across(where(is.numeric), ~ round(., digits = 0)))

write.xlsx(final_row_means_rounded, here("data","processed","final_row_means_rounded.xlsx"))
