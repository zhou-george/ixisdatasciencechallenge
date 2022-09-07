## month by month trends ----
# check ecr trend
db_combined_final_test <- db_combined_final %>%
  mutate(month = as.Date(paste(dim_year, ifelse(nchar(dim_month) == 1, paste0("0", dim_month), dim_month), "01", sep = "-")))

sink(paste0(getwd(), "/data/year_trends.txt"))
summary(lm(db_combined_final_test$sum_sessions ~ db_combined_final_test$month))
summary(lm(db_combined_final_test$sum_transactions ~ db_combined_final_test$month))
summary(lm(db_combined_final_test$mean_ecr ~ db_combined_final_test$month))

# check cart retention trend
summary(lm(db_combined_final_test$sum_QTY ~ db_combined_final_test$month))
summary(lm(db_combined_final_test$addsToCart ~ db_combined_final_test$month))
summary(lm(db_combined_final_test$cart_retention ~ db_combined_final_test$month))
mean(db_combined_final_test$cart_retention)
sink()

## month by month trends device and browser ----
# transform and aggregate based on browser and device
db_scb <- df_sc %>%
  group_by(dim_browser, dim_deviceCategory, dim_month, dim_year) %>%
  mutate(sum_sessions = sum(sessions),
         sum_transactions = sum(transactions),
         sum_QTY = sum(QTY)) %>%
  ungroup() %>%
  select(dim_browser,
         dim_deviceCategory,
         dim_month,
         dim_year,
         sum_sessions,
         sum_transactions,
         sum_QTY) %>%
  distinct()

db_scb_ecr <- df_sc_ecr %>%
  group_by(dim_browser, dim_deviceCategory, dim_month, dim_year) %>%
  mutate(mean_ecr = mean(ecr)) %>%
  ungroup() %>%
  select(dim_browser,
         dim_deviceCategory,
         dim_month,
         dim_year,
         mean_ecr) %>%
  distinct()

db_scb_final <- db_scb %>%
  left_join(db_scb_ecr) %>%
  mutate(mean_ecr = ifelse(is.na(mean_ecr) == TRUE, 0, mean_ecr)) %>%
  mutate(month = as.Date(paste(dim_year, ifelse(nchar(dim_month) == 1, paste0("0", dim_month), dim_month), "01", sep = "-"))) %>%
  mutate(device_browser = paste(dim_deviceCategory, dim_browser, sep = "_")) %>%
  group_by(dim_month, dim_year) %>%
  mutate(total_sessions = sum(sum_sessions),
         total_transactions = sum(sum_transactions),
         total_QTY = sum(sum_QTY),
         total_ecr = sum(mean_ecr)) %>%
  mutate(percent_sessions = sum_sessions/total_sessions,
         percent_transactions = sum_transactions/total_transactions,
         percent_QTY = sum_QTY/total_QTY,
         percent_ecr = mean_ecr/total_ecr) %>%
  ungroup() %>%
  select(device_browser,
         month,
         sum_sessions,
         percent_sessions,
         sum_transactions,
         percent_transactions,
         sum_QTY,
         percent_QTY,
         mean_ecr,
         percent_ecr)

db_scb_totals <- db_scb_final %>%
  mutate(total_sessions = sum(sum_sessions),
         total_transactions = sum(sum_transactions),
         total_QTY = sum(sum_QTY),
         total_ecr = sum(mean_ecr)) %>%
  group_by(device_browser) %>%
  mutate(total_device_sessions = sum(sum_sessions),
         total_device_transactions = sum(sum_transactions),
         total_device_QTY = sum(sum_QTY),
         total_device_ecr = sum(mean_ecr)) %>%
  ungroup() %>%
  select(device_browser,
         contains("total_")) %>%
  distinct() %>%
  mutate(percent_sessions = total_device_sessions/total_sessions,
         percent_transactions = total_device_transactions/total_transactions,
         percent_QTY = total_device_QTY/total_QTY,
         percent_ecr = total_device_ecr/total_ecr) %>%
  select(-contains("total_")) %>%
  arrange(desc(percent_sessions))

# group browser and device metrics with less than 5% for that month into "other"
db_scb_final_sessions <- db_scb_final %>%
  select(device_browser,
         month,
         sum_sessions,
         percent_sessions) %>%
  mutate(device_browser = ifelse(percent_sessions < .05, "other", device_browser)) %>%
  group_by(device_browser, month) %>%
  mutate(sum_sessions = sum(sum_sessions),
         percent_sessions = sum(percent_sessions)) %>%
  ungroup() %>%
  distinct() %>%
  arrange(month,
          desc(sum_sessions))

db_scb_final_transactions <- db_scb_final %>%
  select(device_browser,
         month,
         sum_transactions,
         percent_transactions) %>%
  mutate(device_browser = ifelse(percent_transactions < .05, "other", device_browser)) %>%
  group_by(device_browser, month) %>%
  mutate(sum_transactions = sum(sum_transactions),
         percent_transactions = sum(percent_transactions)) %>%
  ungroup() %>%
  distinct() %>%
  arrange(month,
          desc(sum_transactions))

db_scb_final_qty <- db_scb_final %>%
  select(device_browser,
         month,
         sum_QTY,
         percent_QTY) %>%
  mutate(device_browser = ifelse(percent_QTY < .05, "other", device_browser)) %>%
  group_by(device_browser, month) %>%
  mutate(sum_QTY = sum(sum_QTY),
         percent_QTY = sum(percent_QTY)) %>%
  ungroup() %>%
  distinct() %>%
  arrange(month,
          desc(sum_QTY))

# check ecr any browser or device better than others
db_scb_final_ecr <- db_scb_final %>%
  select(device_browser,
         month,
         mean_ecr,
         percent_ecr) %>%
  mutate(device_browser = ifelse(percent_ecr < .05, "other", device_browser)) %>%
  group_by(device_browser, month) %>%
  mutate(mean_ecr = sum(mean_ecr),
         percent_ecr = sum(percent_ecr)) %>%
  ungroup() %>%
  distinct() %>%
  arrange(month,
          desc(mean_ecr))

# check metrics any device better than others
db_sc_final_graph <- db_sc_final %>%
  mutate(month = as.Date(paste(dim_year, ifelse(nchar(dim_month) == 1, paste0("0", dim_month), dim_month), "01", sep = "-")))

sink(paste0(getwd(), "/data/device_browser_trends.txt"))
summary(lm(filter(db_sc_final_graph, dim_deviceCategory == "desktop")$sum_sessions ~ filter(db_sc_final_graph, dim_deviceCategory == "desktop")$month))
summary(lm(filter(db_sc_final_graph, dim_deviceCategory == "mobile")$sum_sessions ~ filter(db_sc_final_graph, dim_deviceCategory == "mobile")$month))
summary(lm(filter(db_sc_final_graph, dim_deviceCategory == "tablet")$sum_sessions ~ filter(db_sc_final_graph, dim_deviceCategory == "tablet")$month))

summary(lm(filter(db_sc_final_graph, dim_deviceCategory == "desktop")$sum_transactions ~ filter(db_sc_final_graph, dim_deviceCategory == "desktop")$month))
summary(lm(filter(db_sc_final_graph, dim_deviceCategory == "mobile")$sum_transactions ~ filter(db_sc_final_graph, dim_deviceCategory == "mobile")$month))
summary(lm(filter(db_sc_final_graph, dim_deviceCategory == "tablet")$sum_transactions ~ filter(db_sc_final_graph, dim_deviceCategory == "tablet")$month))

summary(lm(filter(db_sc_final_graph, dim_deviceCategory == "desktop")$mean_ecr ~ filter(db_sc_final_graph, dim_deviceCategory == "desktop")$month))
summary(lm(filter(db_sc_final_graph, dim_deviceCategory == "mobile")$mean_ecr ~ filter(db_sc_final_graph, dim_deviceCategory == "mobile")$month))
summary(lm(filter(db_sc_final_graph, dim_deviceCategory == "tablet")$mean_ecr ~ filter(db_sc_final_graph, dim_deviceCategory == "tablet")$month))

summary(lm(filter(db_sc_final_graph, dim_deviceCategory == "desktop")$sum_QTY ~ filter(db_sc_final_graph, dim_deviceCategory == "desktop")$month))
summary(lm(filter(db_sc_final_graph, dim_deviceCategory == "mobile")$sum_QTY ~ filter(db_sc_final_graph, dim_deviceCategory == "mobile")$month))
summary(lm(filter(db_sc_final_graph, dim_deviceCategory == "tablet")$sum_QTY ~ filter(db_sc_final_graph, dim_deviceCategory == "tablet")$month))
sink()

db_sc_totals <- db_sc_final %>%
  mutate(total_sessions = sum(sum_sessions),
         total_transactions = sum(sum_transactions),
         total_QTY = sum(sum_QTY),
         total_ecr = sum(mean_ecr)) %>%
  group_by(dim_deviceCategory) %>%
  mutate(total_device_sessions = sum(sum_sessions),
         total_device_transactions = sum(sum_transactions),
         total_device_QTY = sum(sum_QTY),
         total_device_ecr = sum(mean_ecr)) %>%
  ungroup() %>%
  select(dim_deviceCategory,
         contains("total_")) %>%
  distinct() %>%
  mutate(percent_sessions = total_device_sessions/total_sessions,
         percent_transactions = total_device_transactions/total_transactions,
         percent_QTY = total_device_QTY/total_QTY,
         percent_ecr = total_device_ecr/total_ecr) %>%
  select(-contains("total_"))

## write ----
OUT <- createWorkbook()
addWorksheet(OUT, "device_totals")
addWorksheet(OUT, "device_browser_totals")
writeData(OUT, sheet = "device_totals", x = db_sc_totals)
writeData(OUT, sheet = "device_browser_totals", x = db_scb_totals)
saveWorkbook(OUT, paste0(getwd(), "/data/reference2.xlsx"), overwrite = TRUE)
