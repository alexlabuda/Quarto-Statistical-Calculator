
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


# Update to above code ----------------------------------------------------
library(tidyverse)
library(BayesFactor)
theme_set(theme_minimal())

# Data
visitors    <- c(A = 8500, B = 8500)
conversions <- c(A = 1500, B = 1650)

# Posterior Distributions
prior <- c(1, 1)
post  <- rbind(
  c(conversions["A"] + prior[1], visitors["A"] - conversions["A"] + prior[2]),
  c(conversions["B"] + prior[1], visitors["B"] - conversions["B"] + prior[2])
)

# Simulations
set.seed(123)
simulations <- list(
  A = rbeta(100000, post[1, 1], post[1, 2]),
  B = rbeta(100000, post[2, 1], post[2, 2])
)

# Statistics
prob_B_better_than_A <- mean(simulations$B > simulations$A)
rates                <- conversions / visitors
overall_rate         <- sum(conversions) / sum(visitors)
standard_errors      <- sqrt(rates * (1 - rates) / visitors)

# Z-Score
z_score <- (rates["B"] - rates["A"]) / sqrt(sum(standard_errors^2))
relative_uplift_conversion_rate <- (rates["B"] - rates["A"]) / rates["A"]
p_value <- 2 * pnorm(-abs(z_score))


# Compare conversion rates with error bars
comparison_df <- tibble(
  type          = names(rates),
  rate          = rates,
  std_err       = standard_errors,
  conf_lower    = rates - qnorm(0.99) * standard_errors,
  conf_upper    = rates + qnorm(0.99) * standard_errors
)

comparison_df |> 
  ggplot(aes(x = fct_reorder(type, -rate), y = rate, ymin = conf_lower, ymax = conf_upper, color = type)) +
  geom_crossbar(position = position_dodge(0.5), width = 0.32, size = 0.35) +
  geom_text(aes(label = scales::percent(rate, accuracy = 0.01)), nudge_x =  0.32, size = 3.2, check_overlap = TRUE) +
  scale_y_continuous(labels = scales::percent) +
  scale_color_manual(values = c("A" = "#2E465F", "B" = "#D81B60")) +  # Define custom colors
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



plot_density_comparison <- function(simulations, rates, group_names, group_colors) {
  # Create a long format data frame for all groups
  df <- map2_dfr(simulations, names(simulations), ~data.frame(Group = .y, ConversionRate = .x))
  
  # Start the ggplot
  p <- ggplot(df, aes(x = ConversionRate, fill = Group)) +
    geom_density(alpha = 0.25, linewidth = 0.35) +
    scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
    scale_fill_manual(values = group_colors)
  
  # Add mean lines and labels for each group
  for (i in seq_along(group_names)) {
    group_name <- group_names[i]
    mean_rate  <- rates[group_name]
    color      <- group_colors[i]
    
    p <- p + geom_vline(xintercept = mean_rate, color = color, linetype = "dashed", linewidth = 0.3) +
      annotate("text", x = (mean_rate + 0.0007), y = 50, label = str_c("CR ", group_name, ": ", scales::percent(mean_rate, accuracy = 0.01)),
               color = color, angle = 90, check_overlap = TRUE, size = 3.2)  # Adjust y as needed
  }
  
  # Final touches
  p + labs(x = "", y = "") +
    theme_minimal() + 
    theme(
      legend.position = "none", 
      axis.text.y = element_blank(),
      # panel.grid  = element_blank(),
      panel.grid.major   = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank()
      # panel.grid.minor.y = element_blank()
      )
}

# Usage
group_names <- c("A", "B")
group_colors <- c("A" = "#2E465F", "B" = "#D81B60")
plot_density_comparison(simulations, rates, group_names, group_colors)





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





