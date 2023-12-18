
# Outlier Removal Tool ----------------------------------------------------

# Libraries
library(tidyverse)


# Data --------------------------------------------------------------------

# Load test transactions data

transactions_tbl <- read_csv("test/data/test_transactions_data.csv") |> 
  select(unique_transaction, visitor_id, transaction_revenue) |> 
  mutate(transaction_revenue = transaction_revenue / 1000000)

transactions_tbl |> glimpse()


# Visualize outliers with boxplot
transactions_tbl |> 
  ggplot(aes(x = "", y = transaction_revenue)) +
  geom_boxplot() +
  labs(
    title = "Transactions Revenue Boxplot",
    x     = "",
    y     = "Revenue"
  ) +
  coord_flip() +
  theme_minimal()







