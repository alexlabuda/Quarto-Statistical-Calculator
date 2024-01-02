
# Libraries
library(tidyverse)
library(broom)
library(showtext)
library(DT)
library(BayesFactor)

library(hrbrthemes)


# Values
weekly_traffic  <- 10000
conversion_rate <- 0.05
significance    <- 0.05
power           <- 0.8
mde             <- 0.02
test_type       <- "two.sided"


# DATA PROCESSING ---------------------------------------------------------

## Data process 1
data1 <- 
  seq(weekly_traffic, weekly_traffic * 8, by = weekly_traffic) |> 
    map_df(~ power.prop.test(p1          = conversion_rate,
                             p2          = NULL, 
                             n           = .x, 
                             power       = power, 
                             sig.level   = significance,
                             alternative = test_type,
                             strict      = TRUE
    ) |> 
      tidy()) |> 
    mutate(effect = p2 / p1 - 1,
           total  = n,
           n      = n / 2)



data1 |>
  ggplot(aes(n, effect)) +
  # geom_hline(
  #   yintercept = input$MDE / 100,
  #   linetype = 2,
  #   color = "#FF0266",
  #   alpha = 0.5,
  #   linewidth = 0.8
  # ) +
  geom_bar(stat = "identity", alpha = 0.45, fill = "#48718f") +
  theme_minimal(base_size = 18) +
  scale_y_continuous(labels = scales::percent_format(),
                     limits = c(0, NA)) +
  scale_x_continuous(labels = scales::comma_format()) +
  scale_color_gradient(
    high = "#0077CC",
    low = "#B8E0C5",
    labels = scales::comma_format()
  ) +
  labs(x = "Sample size", y = "Minimum Detectable Effect",
       title = "What is the Minimum detectable effect by sample size?") +
  theme(
    panel.grid   = element_blank(),
    plot.title   = element_text(
      family = "roboto",
      size = 18,
      face = "bold",
      color  = "grey35"
    ),
    axis.title.y = element_text(family = "roboto", size = 16),
    axis.title.x = element_text(family = "roboto", size = 16)
  )
