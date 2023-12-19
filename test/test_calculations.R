# Calculating MDE

# 1 Treatment, 1 Control

library(tidyverse)
library(broom)


# Frequentist -------------------------------------------------------------

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


# Bayesian ----------------------------------------------------------------

library(BayesPPD)

# Sample data



historical <- matrix(0, ncol=3, nrow=2)
historical[1,] <- c(44, 535, 0.3)s
historical[2,] <- c(33, 304, 0.3)


set.seed(1)
n.t_vals <- seq(from=600, to=1000, by=50)
powers <- NULL

for(i in 1:length(n.t_vals)){
  n.t <- n.t_vals[i]
  results <- power.two.grp.fixed.a0(data.type="Bernoulli", 
                                    n.t=n.t, n.c=round(n.t/3), historical=historical,
                                    samp.prior.mu.t=0.092, samp.prior.mu.c=0.092,
                                    prior.mu.t.shape1=0.0001, prior.mu.t.shape2=0.0001, 
                                    prior.mu.c.shape1=0.0001,prior.mu.c.shape2=0.0001,
                                    delta=0.041, N=10000)
  power <- results$`power/type I error`
  powers <- c(powers, power)
}

powers

df <- data.frame(sample_size=n.t_vals, power=powers)

ggplot(data=df, aes(x=sample_size, y=powers)) +
  geom_smooth(method = lm, formula = y ~ x, se = FALSE) +
  geom_point() +
  xlab("Sample Size") +
  ylab("Power")




