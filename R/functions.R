# 定义函数：识别变量类型并重命名
rename_categorical_vars <- function(data, 
                                    binary_max = 2, 
                                    multiclass_max = 10) {
  # 遍历每个变量
  new_names <- sapply(names(data), function(var_name) {
    x <- data[[var_name]]
    unique_values <- na.omit(unique(x))  # 排除NA后取唯一值
    n_unique <- length(unique_values)
    
    # 判断变量类型
    if (n_unique == binary_max) {
      paste0("bin_", var_name)     # 二分类变量添加 bin_
    } else if (n_unique > binary_max & n_unique <= multiclass_max) {
      paste0("cat_", var_name)     # 多分类变量添加 cat_
    } else {
      var_name                     # 其他变量保留原名
    }
  })
  
  # 更新列名
  names(data) <- new_names
  return(data)
}

# 定义函数：处理单个插补数据集，转换变量并求和
get_sums <- function(imp, i) {
  data_imp <- complete(imp, i)  # 提取第i个插补数据集
  # 将因子转换为数值（需确保转换有意义！）
  data_imp$A <- as.numeric(as.character(data_imp$A))
  data_imp$B <- as.numeric(as.character(data_imp$B))
  data_imp$C <- as.numeric(as.character(data_imp$C))
  # 计算各变量的总和
  c(
    sum_A = sum(data_imp$A, na.rm = TRUE),
    sum_B = sum(data_imp$B, na.rm = TRUE),
    sum_C = sum(data_imp$C, na.rm = TRUE)
  )
}