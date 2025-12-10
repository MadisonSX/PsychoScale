backup_workspace <- function() {
  # 创建备份目录
  backup_dir <- here::here("backups")
  if (!dir.exists(backup_dir)) dir.create(backup_dir, recursive = TRUE)
  
  # 生成新备份
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  backup_file <- file.path(backup_dir, paste0("backup_", timestamp, ".RData"))
  save.image(file = backup_file)
  
  # 获取所有备份并按时间排序
  files <- list.files(backup_dir, "\\.RData$", full.names = TRUE)
  file_mtime <- file.info(files)$mtime
  sorted_files <- files[order(file_mtime, decreasing = TRUE)]  # 从新到旧排序
  
  # 计算保留范围
  seven_days_ago <- Sys.time() - 7 * 24 * 60 * 60
  is_recent <- file_mtime >= seven_days_ago  # 7天内标记
  keep_min <- max(10 - sum(is_recent), 0)    # 需补充的最小数量
  
  # 保留文件 = 7天内所有 + 必要的最新旧备份
  keep_files <- c(
    sorted_files[is_recent],                 # 所有7天内
    sorted_files[!is_recent][seq_len(keep_min)]  # 补充旧备份
  )
  
  # 删除其他文件
  delete_files <- setdiff(files, keep_files)
  if (length(delete_files) > 0) {
    removed <- file.remove(delete_files)
    message("清理备份：删除 ", sum(removed), " 个文件，保留 ", length(keep_files), " 个（7天内:", sum(is_recent), "+补充:", keep_min, "）")
  }
  
  message("备份完成：", backup_file)
}

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