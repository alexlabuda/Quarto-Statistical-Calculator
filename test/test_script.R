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
weekly_traffic           <- 20000
weekly_conversions       <- 200
baseline_conversion_rate <- 0.01
number_of_variants       <- 2
confidence_level         <- 0.95
statistical_power        <- 0.80

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


# Parameters
alpha = 0.05     # Significance level (for a two-sided test)
power = 0.80     # Power
sigma = 2.5       # Standard deviation (example value)
n     = 10000      # Sample size per group

# Calculate z-scores
z_alpha = qnorm(1 - alpha / 2)  # Z-score for alpha
z_beta = qnorm(power)          # Z-score for beta

# Calculate MDE
mde = (z_alpha + z_beta) * sqrt(2 * sigma^2 / n)

# Output MDE
print(mde)



ab_test_calculator <- function(weekly_traffic, weekly_conversions, num_variants, baseline_conv_rate, confidence_level, statistical_power, num_weeks) {
  results <- data.frame(Week = integer(), MDE = numeric(), VisitorsPerVariant = numeric())
  
  for (week in 1:num_weeks) {
    # Calculate cumulative visitors per variant
    visitors_per_variant = (weekly_traffic * week) / num_variants
    
    # Use the pwr package or similar to calculate MDE
    # This is a simplified example, and you'll need to adjust it based on your specific formula
    effect_size <- pwr.2p.test(h = baseline_conv_rate, n = visitors_per_variant, sig.level = confidence_level, power = statistical_power)$h
    
    results <- rbind(results, data.frame(Week = week, MDE = effect_size, VisitorsPerVariant = visitors_per_variant))
  }
  
  return(results)
}


ab_test_calculator <- function(weekly_traffic, weekly_conversions, num_variants, baseline_conv_rate, confidence_level, statistical_power, num_weeks) {
  results <- data.frame(Week = integer(), MDE = numeric(), VisitorsPerVariant = numeric())
  
  for (week in 1:num_weeks) {
    visitors_per_variant = (weekly_traffic * week) / num_variants
    
    # Calculate MDE, setting 'h' as NULL
    mde_result <- pwr.2p.test(h = NULL, n = visitors_per_variant, sig.level = confidence_level, power = statistical_power)
    mde <- mde_result$h
    
    results <- rbind(results, data.frame(Week = week, MDE = mde, VisitorsPerVariant = visitors_per_variant))
  }
  
  return(results)
}

ab_test_calculator(
  weekly_traffic = 20000, 
  weekly_conversions = 500, 
  num_variants = 2, 
  baseline_conv_rate = 0.025, 
  confidence_level = 0.95, 
  statistical_power = 0.8, 
  num_weeks = 6
  )


# Set parameters
baseline_conversion_rate <- 0.05
sample_size_per_group <- 20000
significance_level <- 0.05
power <- 0.80

# Calculate effect size
effect_size <- pwr.2p.test(h           = NULL, 
                           n           = sample_size_per_group, 
                           sig.level   = significance_level, 
                           power       = power, 
                           alternative = "greater")$h

# Convert effect size to MDE
mde <- baseline_conversion_rate + effect_size

# Print MDE
mde * 100




