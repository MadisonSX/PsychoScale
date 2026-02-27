# 脚本名称: 06_合并表格.R
# 功能: 读取两份结果表，去重后按ID全连接合并，并输出合并结果表。
# 输入: data/raw/EpiData_final.xlsx
#       data/raw/RawData_final.xlsx
# 输出: data/processed/merged_result.xlsx

# 1. 读取两个Excel文件
suppressMessages(df1 <- read_excel(here("data", "raw", "EpiData_final.xlsx")))
suppressMessages(df2 <- read_excel(here("data", "raw", "RawData_final.xlsx")))
df1$ID <- as.numeric(df1$ID)

# 2. 检查重复ID（以df1为例，df2同理）
duplicates_df1 <- df1 %>% 
  group_by(ID) %>% 
  dplyr::filter(n() > 1) %>% 
  arrange(ID)
duplicates_df2 <- df2 %>% 
  group_by(ID) %>% 
  dplyr::filter(n() > 1) %>% 
  arrange(ID)

# 3. 处理重复ID策略（保留最后出现的记录）
clean_df1 <- df1 %>%
  group_by(ID) %>%
  slice_head(n = 1) %>%  # 每个ID保留第一条记录
  # slice_tail(n = 1) %>%  # 每个ID保留最后一条记录
  ungroup()
clean_df2 <- df2 %>%
  group_by(ID) %>%
  slice_head(n = 1) %>%  # 每个ID保留第一条记录
  ungroup()

# 4. 全连接合并（保留所有ID）
merged_df <- full_join(clean_df1, clean_df2, by = "ID", suffix = c("_file1", "_file2"))

# 5. 比对数据
match_count <- merged_df %>%
  dplyr::filter(
    grade == GRADE,
    class == CLASS,
    sex == SEX,
    age == AGE
  ) %>% 
  nrow()
matched_df <- merged_df %>%
  dplyr::filter(
    grade == GRADE,
    class == CLASS,
    sex == SEX,
    age == AGE
  )

# 7. 保存结果
write_xlsx(
  merged_df,          # 要导出的数据框
  here("data", "processed", "merged_result.xlsx"),    # 输出路径
)

# 备份 ----
backup_workspace()
