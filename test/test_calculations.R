# Calculating MDE

# 1 Treatment, 1 Control

library(tidyverse)
library(broom)

weekly_traffic <- 5000
p1             <- 10  # in %
p2             <- NULL
power          <- 80  # in %
sig_level      <- 5   # in %
test_type      <- "two.sided"


data1 <- 
  seq(weekly_traffic, weekly_traffic * 8, by = weekly_traffic) |> 
    map_df(~ power.prop.test(p1          = p1 / 100,
                             p2          = p2, 
                             n           = .x, 
                             power       = power / 100, 
                             sig.level   = sig_level / 100,
                             alternative = test_type,
                             strict      = TRUE) |> 
             tidy()) |> 
    mutate(mde    = p2 / p1 - 1,
           total  = n,
           n      = n / 2)






data <- eventReactive(eventExpr = input$apply, 
                      valueExpr = {
                        seq(input$WeeklyTraffic / 2, input$WeeklyTraffic / 2 * 8, by = input$WeeklyTraffic / 2) %>%
                          map_df(~ power.prop.test(
                            p1          = input$Baseline / 100,
                            p2          = NULL, 
                            n           = .x, 
                            power       = input$Power / 100, 
                            sig.level   = input$SigLevel / 100,
                            alternative = input$Test_type) %>%
                              tidy()) %>%
                          mutate(effect = scales::percent(p2 / p1 - 1),
                                 total  = scales::comma(n * 2),
                                 n      = scales::comma(n)) |> 
                          mutate(Weeks  = str_c(row_number(), " ", "Weeks")) |> 
                          mutate(difference = abs(as.numeric(sub("%", "", effect)) - input$MDE))
                      },
                      ignoreNULL = FALSE)
