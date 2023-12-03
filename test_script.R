# Install necessary packages
if (!require(pwr)) install.packages("pwr")
library(pwr)

# Inputs
weekly_traffic     <- 20000
weekly_conversions <- 200
baseline_conversion_rate <- 0.01
confidence_level   <- 0.95
statistical_power  <- 0.80
number_of_variants <- 2

# Function to calculate MDE and sample size per variant
calculate_values <- function(weeks) {
  total_traffic <- weekly_traffic * weeks
  baseline_conversions <- weekly_conversions * weeks
  
  # Assuming baseline conversion rates and conversions remain constant
  # Adjust p1 as the baseline conversion rate for the control group
  # p2 is the conversion rate we want to be able to detect
  # This is an iterative process to find the minimum p2 that meets the power requirement
  
  p1 <- baseline_conversion_rate
  p2 <- p1 # Starting value for p2, will be adjusted iteratively
  
  # Initialize power for the initial p2
  observed_power <- pwr.2p.test(h = ES.h(p1, p2), n = total_traffic / number_of_variants,
                                sig.level = 1 - confidence_level)$power
  
  # Increment p2 until the desired power is achieved
  while (observed_power < statistical_power) {
    p2 <- p2 + 0.0001 # Adjust the increment value as needed for precision
    observed_power <- pwr.2p.test(h = ES.h(p1, p2), n = total_traffic / number_of_variants,
                                  sig.level = 1 - confidence_level)$power
  }
  
  # Calculate MDE based on final p2
  mde <- (p2 - p1) / p1 * 100 # MDE as a percentage
  
  # Output MDE and visitors per variant
  list(MDE = mde, VisitorsPerVariant = total_traffic / number_of_variants)
}

# Apply the function over the range of weeks
results <- lapply(1:6, calculate_values)

# Display the results
results




## 2nd result

library(pwr)

# Inputs from the user
weekly_traffic <- 20000
weekly_conversions <- 200
baseline_conversion_rate <- 0.01
number_of_variants <- 2
confidence_level <- 0.95
statistical_power <- 0.80

# Function to calculate the effect size needed for power calculation
# This function will be used within the sample size calculation to find the MDE
calculate_effect_size <- function(p1, p2) {
  return(ES.h(p1, p2))
}

# Function to calculate sample size and MDE iteratively
calculate_sample_size_and_mde <- function(weeks) {
  n_baseline <- weekly_traffic * weeks / number_of_variants
  p1 <- baseline_conversion_rate
  p2 <- p1 # starting value for p2, to be incremented to find MDE
  
  # Start with a difference of 0 and increment until the desired power is achieved
  while(TRUE) {
    h <- calculate_effect_size(p1, p2)
    power_calc <- pwr.2p2n.test(h = h, n1 = n_baseline, n2 = n_baseline, sig.level = 1 - confidence_level)
    
    if (power_calc$power >= statistical_power) break
    p2 <- p2 + 0.0001 # increment p2 slightly
  }
  
  mde <- (p2 - p1) / p1 * 100 # MDE as a percentage
  visitors_per_variant <- ceiling(power_calc$n1) # rounding up to ensure required power
  
  return(list(MDE = mde, VisitorsPerVariant = visitors_per_variant))
}

# Calculate MDE and sample size for 1 to 6 weeks
results <- lapply(1:6, calculate_sample_size_and_mde)

# Output results
results

