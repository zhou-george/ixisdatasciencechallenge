if (require("ggplot2")) {"ggplot2 installed"} else {install.packages("ggplot2")}

## cart retention ----
db_combined_final_graph <- db_combined_final_test %>%
  gather("metric", "value", sum_sessions:cart_retention_rel) %>%
  filter(!grepl("ecr", metric),
         !grepl("cart_retention", metric),
         !grepl("_abs", metric),
         !grepl("_rel", metric))

db_combined_final_graph_cr <- db_combined_final_graph %>%
  filter(metric %in% c("sum_QTY", "addsToCart"))

tiff(paste0(getwd(), "/data/month_over_month_cr.tiff"), units="in", width = 20, height = 10, res=300)
ggplot(db_combined_final_graph_cr, aes(x = month, y = value, group = metric, color = metric)) +
  geom_line() +
  geom_point() +
  xlab("Month") +
  ylab("Number of")
dev.off()

## stacked bar charts ----
plot_bar_stacked <- function(df, string) {
  tiff(paste0(getwd(), "/data/browser_device_", string, ".tiff"), units="in", width = 20, height = 10, res=300)
  ggplot(df, aes(y = value, x = month, fill = device_browser)) +
    geom_bar(position = "stack", stat = "identity") +
    xlab("Month") +
    ylab("Number of")
}

# by device and browser
db_scb_final_sessions_graph <- db_scb_final_sessions %>%
  select(-percent_sessions) %>%
  gather("metric", "value", sum_sessions)

plot_bar_stacked(db_scb_final_sessions_graph, "sessions")
dev.off()

db_scb_final_qty_graph <- db_scb_final_qty %>%
  select(-percent_QTY) %>%
  gather("metric", "value", sum_QTY)

plot_bar_stacked(db_scb_final_qty_graph, "qty")
dev.off()
