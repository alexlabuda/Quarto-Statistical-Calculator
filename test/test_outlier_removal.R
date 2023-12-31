
# Outlier Removal Tool ----------------------------------------------------

# Libraries
library(tidyverse)
source("functions/helper_functions.R")

# Data --------------------------------------------------------------------

# Load test transactions data

transactions_tbl <- read_csv("test/data/test_transactions_data.csv") |> 
  select(unique_transaction, visitor_id, transaction_revenue) |> 
  mutate(transaction_revenue = transaction_revenue / 1000000)

transactions_tbl |> glimpse()


# Visualize outliers with boxplot
transactions_tbl |> 
  outlier_winsorize("transaction_revenue") |> 
  ggplot(aes(x = "", y = transaction_revenue)) +
  geom_boxplot() +
  labs(
    title = "Transactions Revenue Boxplot",
    x     = "",
    y     = "Revenue"
  ) +
  coord_flip() +
  theme_minimal()

# Test outlier removal function
transactions_tbl |> 
  outlier_removal("transaction_revenue") |> 
  ggplot(aes(x = "", y = transaction_revenue)) +
  geom_boxplot() +
  labs(
    title = "Transactions Revenue Boxplot",
    x     = "",
    y     = "Revenue"
  ) +
  coord_flip() +
  theme_minimal()





