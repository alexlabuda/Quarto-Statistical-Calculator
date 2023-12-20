
# POST TEST CALCULATORS ---------------------------------------------------

library(tidyverse)
library(BayesFactor)
theme_set(theme_minimal())


visitors_a    <- 8500
visitors_b    <- 8500
conversions_a <- 1500
conversions_b <- 1650

hypothesis <- ""
confidence <- 0.95


# Calculate the posterior distribution of the conversion rate for each group
# Using a uniform prior Beta(1,1)
prior <- c(1, 1)

# Calculating posterior distributions
post_A <- c(conversions_a + prior[1], visitors_a - conversions_a + prior[2])
post_B <- c(conversions_b + prior[1], visitors_b - conversions_b + prior[2])

# Simulating from the posterior distributions
set.seed(123)
sim_A <- rbeta(10000, post_A[1], post_A[2])
sim_B <- rbeta(10000, post_B[1], post_B[2])

# Probability that B is better than A
prob_B_better_than_A <- mean(sim_B > sim_A)

# Calculate other statistics
rate_A <- conversions_a / visitors_a
rate_B <- conversions_b / visitors_b

# Pooled conversion rate
overall_rate <- (conversions_a + conversions_b) / (visitors_a + visitors_b)

# Z-Score for p-value calculation
z_score <- (rate_B - rate_A) / sqrt(overall_rate * (1 - overall_rate) * (1/visitors_a + 1/visitors_b))

standard_error_a <- sqrt(rate_A * (1 - rate_A) / visitors_a)
standard_error_b <- sqrt(rate_B * (1 - rate_B) / visitors_b)

z_score <- (rate_B - rate_A) / sqrt(standard_error_a^2 + standard_error_b^2)

std_error_of_difference <- sqrt(standard_error_a^2 + standard_error_b^2)

relative_uplift_conversion_rate <- (rate_B - rate_A) / rate_A



# Plotting ----------------------------------------------------------------

max_x_lim <- max(max(sim_A), max(sim_B))
min_x_lim <- min(min(sim_A), min(sim_B))

mean_A <- mean(sim_A)
mean_B <- mean(sim_B)


# Create a sequence of conversion rates
x <- seq(0, 1, length.out = 1000)

# Calculate densities
density_A <- dbeta(x, post_A[1], post_A[2])
density_B <- dbeta(x, post_B[1], post_B[2])

# Create a data frame for plotting
df <- data.frame(x, Group_A = density_A, Group_B = density_B)


df |>
  ggplot(aes(x)) +
  # geom_line(aes(y = Group_A, color = "Group A"), linewidth = 0.8) +
  # geom_line(aes(y = Group_B, color = "Group B"), linewidth = 0.8) +
  geom_area(aes(y = Group_A, fill = "Group A"), alpha = 0.45) +  # Using geom_area for Group A
  geom_area(aes(y = Group_B, fill = "Group B"), alpha = 0.45) +  # Using geom_area for Group B
  scale_fill_manual(
    values = c("Group A" = "#2E465F", "Group B" = "#D81B60"),
    name   = "Groups:"
  ) +
  labs(
    title = NULL,
    x     = "Conversion Rate", 
    y     = "Density"
  ) +
  xlim(min_x_lim, max_x_lim) +
  theme(legend.position = "bottom")

# Add vertical lines for the means
df |>
  ggplot(aes(x)) +
  geom_area(aes(y = Group_A, fill = "Group A"), alpha = 0.45) +  # Using geom_area for Group A
  geom_area(aes(y = Group_B, fill = "Group B"), alpha = 0.45) +  # Using geom_area for Group B
  geom_vline(xintercept = mean_A, color = "#2E465F", linetype = "dashed", size = 0.5) + # Vertical line for Group A
  geom_vline(xintercept = mean_B, color = "#D81B60", linetype = "dashed", size = 0.5) + # Vertical line for Group B
  scale_fill_manual(
    values = c("Group A" = "#2E465F", "Group B" = "#D81B60"),
    name   = "Groups:"
  ) +
  labs(
    title = NULL,
    x     = "Conversion Rate", 
    y     = "Density"
  ) +
  xlim(min_x_lim, max_x_lim) +
  theme(legend.position = "bottom")

# Add labels to the mean of each group

df |>
  ggplot(aes(x)) +
  geom_area(aes(y = Group_A, fill = "Group A"), alpha = 0.2, color = "#2E465F", size = 0.3) +  # Using geom_area for Group A
  geom_area(aes(y = Group_B, fill = "Group B"), alpha = 0.2, color = "#D81B60", size = 0.3) +  # Using geom_area for Group B
  # geom_line(aes(y = Group_A), linewidth = 0.2) +
  # geom_line(aes(y = Group_B), linewidth = 0.2) +
  geom_vline(xintercept = mean_A, color = "#2E465F", linetype = "dashed", size = 0.25) + # Vertical line for Group A
  geom_vline(xintercept = mean_B, color = "#D81B60", linetype = "dashed", size = 0.25) + # Vertical line for Group B
  geom_text(aes(x = mean_A, y = max(Group_A), 
                label = str_c("CR B: ", scales::percent(round(mean_A, 4), accuracy = 0.01))), 
            color = "#2E465F", angle = 90, vjust = -0.5, hjust = 2, check_overlap = TRUE,
            size = 3.2) + # Label for Group A
  geom_text(aes(x = mean_B, y = max(Group_B), 
                label = str_c("CR A: ", scales::percent(round(mean_B, 4), accuracy = 0.01))), 
            color = "#D81B60", angle = 90, vjust = -0.5, hjust = 2, check_overlap = TRUE,
            size = 3.2) + # Label for Group B
  scale_fill_manual(
    values = c("Group A" = "#2E465F", "Group B" = "#D81B60"),
    name   = "Groups:"
  ) +
  labs(
    title = "The expected distributions of variation A and B",
    x     = NULL, 
    y     = ""
  ) +
  xlim(min_x_lim, max_x_lim) +
  theme(
    legend.position  = "none",
    legend.text      = element_text(size = 8),
    legend.title     = element_text(size = 10),
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    axis.text.x      = element_blank(),       # Remove x-axis text
    plot.title       = element_text(hjust = -0.08, vjust = 1.5, size = 10, face = "bold")
    )


tibble(
  type         = c("Group A", "Group B"),
  rate         = c(rate_A, rate_B),
  std_err      = c(standard_error_a, standard_error_b),
  conf_upper   = c(qnorm(0.90) * standard_error_a + rate_A, qnorm(0.90) * standard_error_b + rate_B),
  conf_lower   = c(qnorm(0.1) * standard_error_a + rate_A, qnorm(0.1) * standard_error_b + rate_B)
) |> 
  # plot rate and error bars for each type
  ggplot(aes(fct_reorder(type, -rate), rate, color = type)) +
  geom_crossbar(aes(ymin = conf_lower, ymax = conf_upper), width = 0.32, show.legend = FALSE, size = 0.35) +
  geom_text(aes(label = scales::percent(rate, accuracy = 0.01)), nudge_x =  0.32, size = 3.2, check_overlap = TRUE) +
  scale_y_continuous(labels = scales::percent) +
  scale_color_manual(values = c("Group A" = "#2E465F", "Group B" = "#D81B60")) +  # Define custom colors
  labs(
    title = "Conversion Rate",
    x     = "", 
    y     = NULL
  ) +
  coord_flip() +
  theme(
    legend.position = "none",
    panel.grid = element_blank(),
    axis.text.x = element_blank(),
    # update background color
    plot.background = element_rect(fill = "#F5F5F5", color = NA)
    )



# Test BayesAB ------------------------------------------------------------

library(bayesAB)

# Parameters
visitors_a    <- 8500
visitors_b    <- 8500
conversions_a <- 1500
conversions_b <- 1650

# Create Dataset A
# First create a vector with 1's equal to the number of conversions
data_a <- rep(1, conversions_a)
# Then add 0's to make the length of the vector equal to the number of visitors
data_a <- c(data_a, rep(0, visitors_a - conversions_a))

# Create Dataset B
# Similar approach for group B
data_b <- rep(1, conversions_b)
data_b <- c(data_b, rep(0, visitors_b - conversions_b))

AB1 <- bayesTest(data_a, data_b, priors = c('alpha' = 1, 'beta' = 1), n_samples = 1e5, distribution = 'bernoulli')

summary(AB1)

plot(AB1)


# Bayes Factor ------------------------------------------------------------

library(BayesFactor)
library(coda)
library(rjags)

# Parameters
visitors_a    <- 8500
visitors_b    <- 8500
conversions_a <- 1500
conversions_b <- 1650

hypothesis <- "two-sided"  # Choose between "one-sided" and "two-sided"
confidence <- 0.95         # Choose between 0.90, 0.95, and 0.99


result <- bayes.prop.test(c(conversions_a, conversions_b), c(visitors_a, visitors_b), 
                          nullInterval = ifelse(hypothesis == "one-sided", c(0, Inf), c(-Inf, Inf)), 
                          priorStrength = 1)





