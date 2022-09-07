if (require("tidyverse")) {"tidyverse installed"} else {install.packages("tidyverse")}
if (require("data.table")) {"data.table installed"} else {install.packages("data.table")}
if (require("lubridate")) {"lubridate installed"} else {install.packages("lubridate")}
if (require("openxlsx")) {"openxlsx installed"} else {install.packages("openxlsx")}

df_atc0 <- read_csv(paste0(getwd(), "/data/DataAnalyst_Ecom_data_addsToCart.csv"))
df_sc0 <- read_csv(paste0(getwd(), "/data/DataAnalyst_Ecom_data_sessionCounts.csv"))

## transform ----
df_sc <- df_sc0 %>%
  mutate(dim_date = mdy(dim_date),
         dim_month = month(dim_date),
         dim_year = year(dim_date),
         ecr = transactions/sessions)

## table 1 ----
db_sc <- df_sc %>%
  group_by(dim_deviceCategory, dim_month, dim_year) %>%
  mutate(sum_sessions = sum(sessions),
         sum_transactions = sum(transactions),
         sum_QTY = sum(QTY)) %>%
  ungroup() %>%
  select(dim_deviceCategory,
         dim_month,
         dim_year,
         sum_sessions,
         sum_transactions,
         sum_QTY) %>%
  distinct()

df_sc_ecr <- df_sc %>%
  filter(!is.nan(ecr),
         !is.infinite(ecr))

db_sc_ecr <- df_sc_ecr %>%
  group_by(dim_deviceCategory, dim_month, dim_year) %>%
  mutate(mean_ecr = mean(ecr)) %>%
  ungroup() %>%
  select(dim_deviceCategory,
         dim_month,
         dim_year,
         mean_ecr) %>%
  distinct()

db_sc_final <- db_sc %>%
  left_join(db_sc_ecr) %>%
  arrange(dim_deviceCategory, dim_year, dim_month)

## table 2 ----
df_combined_sum <- df_sc %>%
  select(-dim_deviceCategory,
         -dim_date) %>%
  group_by(dim_month, dim_year) %>%
  mutate(sum_sessions = sum(sessions),
         sum_transactions = sum(transactions),
         sum_QTY = sum(QTY)) %>%
  ungroup() %>%
  select(dim_month,
         dim_year,
         sum_sessions,
         sum_transactions,
         sum_QTY) %>%
  distinct()

df_combined_ecr <- df_sc_ecr %>%
  select(-dim_deviceCategory,
         -dim_date) %>%
  filter(!is.nan(ecr),
         !is.infinite(ecr)) %>%
  group_by(dim_month, dim_year) %>%
  mutate(mean_ecr = mean(ecr)) %>%
  ungroup() %>%
  select(dim_month,
         dim_year,
         mean_ecr) %>%
  distinct()

df_combined <- df_combined_sum %>%
  left_join(df_combined_ecr) %>%
  left_join(df_atc0) %>%
  mutate(cart_retention = sum_QTY/addsToCart) %>%
  arrange(dim_year, dim_month)

df_combined <- setDT(df_combined)
db_combined <- df_combined[, paste0(colnames(df_combined),"_prev") := shift(.SD, 1, NA, "lag"), .SDcols = colnames(df_combined)]

db_combined_final <- db_combined %>%
  mutate(sessions_abs = sum_sessions - sum_sessions_prev,
         sessions_rel = (sum_sessions - sum_sessions_prev)/sum_sessions,
         transactions_abs = sum_transactions - sum_transactions_prev,
         transactions_rel = (sum_transactions - sum_transactions_prev)/sum_transactions,
         QTY_abs = sum_QTY - sum_QTY_prev,
         QTY_rel = (sum_QTY - sum_QTY_prev)/sum_QTY,
         ecr_abs = mean_ecr - mean_ecr_prev,
         ecr_rel = (mean_ecr - mean_ecr_prev)/mean_ecr,
         addsToCart_abs = addsToCart - addsToCart_prev,
         addsToCart_rel = (addsToCart - addsToCart_prev)/addsToCart,
         cart_retention_abs = cart_retention - cart_retention_prev,
         cart_retention_rel = (cart_retention - cart_retention_prev)/cart_retention) %>%
  select(-contains("_prev")) %>%
  select(dim_month,
         dim_year,
         sum_sessions,
         sessions_abs,
         sessions_rel,
         sum_transactions,
         transactions_abs,
         transactions_rel,
         sum_QTY,
         QTY_abs,
         QTY_rel,
         mean_ecr,
         ecr_abs,
         ecr_rel,
         addsToCart,
         addsToCart_abs,
         addsToCart_rel,
         cart_retention,
         cart_retention_abs,
         cart_retention_rel)

## write ----
OUT <- createWorkbook()
addWorksheet(OUT, "month_and_device")
addWorksheet(OUT, "month_by_month")
writeData(OUT, sheet = "month_and_device", x = db_sc_final)
writeData(OUT, sheet = "month_by_month", x = db_combined_final)
saveWorkbook(OUT, paste0(getwd(), "/data/reference.xlsx"), overwrite = TRUE)
