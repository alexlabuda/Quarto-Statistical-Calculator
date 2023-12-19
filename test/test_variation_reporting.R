
# Variation Reporting -----------------------------------------------------

# Libraries
library(tidyverse)
library(gt)
source("functions/helper_functions.R")

# Data --------------------------------------------------------------------

# transactions_tbl <- 
#   read_csv("test/data/test_transactions_data.csv") |> 
#   select(variation_id, unique_transaction, visitor_id, transaction_revenue) |> 
#   mutate(transaction_revenue = transaction_revenue / 1000000)

# Test file from downloads
transactions_tbl <- 
  read_csv("~/Downloads/test_transactions_sonesta.csv") |> 
  select(variation_id, unique_transaction, visitor_id, transaction_revenue) |> 
  mutate(transaction_revenue = transaction_revenue / 1000000) |> 
  mutate(variation_type = if_else(variation_id == 0, "Original", "2nd Sticky Bar"))

all_visitor_tbl <- read_csv("~/Downloads/transaction_sonesta_ab_testing.csv") |> 
  select(variation_id, visitor_id, event_time) |> 
  mutate(variation_type = if_else(variation_id == 0, "Original", "2nd Sticky Bar"),
         event_time     = date(event_time))

all_visitors_summary_tbl <- all_visitor_tbl |> 
  reframe(
    unique_visitors_total = n_distinct(visitor_id),
    .by     = variation_type
  )


# Variation Reporting -----------------------------------------------------

summary_original_tbl <- transactions_tbl |> 
  reframe(
    VISITORS           = n_distinct(visitor_id),
    TOTAL_TRANSACTION  = n_distinct(unique_transaction),
    REVENUE            = sum(transaction_revenue, na.rm  = TRUE),
    AOV                = REVENUE / TOTAL_TRANSACTION,
    .by            = variation_type
  ) |> 
  left_join(all_visitors_summary_tbl, by = "variation_type") |> 
  mutate(
    RPV              = REVENUE / unique_visitors_total,
    transaction_rate = TOTAL_TRANSACTION / unique_visitors_total
  ) |> 
  select(`VARIATION`        = variation_type,
         `TRANSACTION RATE` = transaction_rate,
         `AVG ORDER VALUE`  = AOV,
         REVENUE,
         `REVENUE PER USER` = RPV)

gt_summary_table <- summary_original_tbl |> 
  gt(rowname_col = "VARIATION") |> 
  tab_header(
    title    = md("**Test Variation Reporting**"),
    subtitle = md("**Sonesta** - 2023-12-04 to 2023-12-18")
  ) |> 
  fmt_percent(
    columns  = `TRANSACTION RATE`,
    decimals = 2
  ) |> 
  fmt_currency(
    columns  = c(`REVENUE PER USER`, `AVG ORDER VALUE`),
    currency = "USD",
    decimals = 2
  ) |> 
  fmt_currency(
    columns  = REVENUE,
    currency = "USD",
    decimals = 0
  ) |> 
  tab_options(
    column_labels.font.size    = px(16),
    table.font.size            = px(16),
    heading.title.font.size    = px(24),
    heading.subtitle.font.size = px(16),
    heading.padding            = 6,
    column_labels.padding      = 16,
    column_labels.padding.horizontal = 20,
    data_row.padding           = 15
  ) |>
  opt_table_font(
      font = c(
        google_font(name = "Fira Sans"),
        default_fonts()
      )
    )
gtsave(gt_summary_table, filename = "test/artifacts/summary_table_sonesta.html")
  

# Remove Outliers ---------------------------------------------------------

transactions_winsorized_tbl <- transactions_tbl |> 
  outlier_winsorize("transaction_revenue")
  
summary_winsored_tbl <- transactions_winsorized_tbl |> 
  reframe(
    VISITORS           = n_distinct(visitor_id),
    TOTAL_TRANSACTION  = n(),
    REVENUE            = sum(transaction_revenue, na.rm  = TRUE),
    AOV                = REVENUE / TOTAL_TRANSACTION,
    .by            = variation_type
  ) |> 
  left_join(all_visitors_summary_tbl, by = "variation_type") |> 
  mutate(
    RPV              = REVENUE / unique_visitors_total,
    transaction_rate = TOTAL_TRANSACTION / unique_visitors_total
  ) |> 
  select(`VARIATION`        = variation_type,
         `TRANSACTION RATE` = transaction_rate,
         `AVG ORDER VALUE`  = AOV,
         REVENUE,
         `REVENUE PER USER` = RPV)

gt_summary_winsored_table <- summary_winsored_tbl |> 
  gt(rowname_col = "VARIATION") |> 
  tab_header(
    title    = md("**Test Variation Reporting (Outliers Removed)**"),
    subtitle = md("**Sonesta** - 2023-12-04 to 2023-12-18")
  ) |> 
  fmt_percent(
    columns  = `TRANSACTION RATE`,
    decimals = 2
  ) |> 
  fmt_currency(
    columns  = c(`REVENUE PER USER`, `AVG ORDER VALUE`),
    currency = "USD",
    decimals = 2
  ) |> 
  fmt_currency(
    columns  = REVENUE,
    currency = "USD",
    decimals = 0
  ) |> 
  tab_options(
    column_labels.font.size    = px(16),
    table.font.size            = px(16),
    heading.title.font.size    = px(24),
    heading.subtitle.font.size = px(16),
    heading.padding            = 6,
    column_labels.padding      = 16,
    column_labels.padding.horizontal = 20,
    data_row.padding           = 15
  ) |> 
  opt_table_font(
    font = c(
      google_font(name = "Fira Sans"),
      default_fonts()
    )
  )


gtsave(gt_summary_winsored_table, filename = "test/artifacts/summary_winsored_table_sonesta.html")


# Boxplots ----------------------------------------------------------------

# Create a boxplot of transactions by variation type
transactions_tbl |> 
  ggplot(aes(x = variation_type, y = transaction_revenue)) +
  geom_boxplot(outlier.alpha = 0.5) +
  labs(
    title   = "Transaction Revenue by Variation Type",
    x       = "",
    y       = "Transaction Revenue (USD)",
    caption = "Sonesta AB Test - 2023-12-04 to 2023-12-18"
  ) +
  scale_y_continuous(labels = scales::dollar_format()) +
  theme_minimal() +
  theme(
    plot.title   = element_text(size = 20, face = "bold", hjust = -0.3),
    axis.text    = element_text(size = 11),
    axis.title.x = element_text(vjust = -1.6, size = 12),
    axis.text.x  = element_text(),
    legend.title = element_text(size = 14),
    legend.text  = element_text(size = 12),
    # adjust caption
    plot.caption = element_text(vjust = -1.5, size = 10, color = "grey15")
  ) +
  coord_flip()

ggsave("test/artifacts/boxplot_sonesta_original_data.jpg", width = 10, height = 6, dpi = 300)

# Create the same boxplot using the transactions_winsorized_tbl 
transactions_winsorized_tbl |> 
  ggplot(aes(x = variation_type, y = transaction_revenue)) +
  geom_boxplot(outlier.alpha = 0.5) +
  labs(
    title   = "Transaction Revenue by Variation Type (Outliers Removed)",
    x       = "",
    y       = "Transaction Revenue (USD)",
    caption = "Sonesta AB Test - 2023-12-04 to 2023-12-18"
  ) +
  scale_y_continuous(labels = scales::dollar_format()) +
  theme_minimal() +
  theme(
    plot.title   = element_text(size = 20, face = "bold", hjust = -1.3),
    axis.text    = element_text(size = 11),
    axis.title.x = element_text(vjust = -1.6, size = 12),
    axis.text.x  = element_text(),
    legend.title = element_text(size = 14),
    legend.text  = element_text(size = 12),
    # adjust caption
    plot.caption = element_text(vjust = -1.5, size = 10, color = "grey15")
  ) +
  coord_flip()

ggsave("test/artifacts/boxplot_sonesta_winsored_data.jpg", width = 10, height = 6, dpi = 300)

