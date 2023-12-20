# AB Post-Test evaluation functions ----

# Function for Density Comparison Plot
plot_density_comparison <- function(simulations, rates, group_names, group_colors) {
  df <- map2_dfr(simulations, names(simulations), ~data.frame(Group = .y, ConversionRate = .x))
  p <- ggplot(df, aes(x = ConversionRate, fill = Group)) +
    geom_density(alpha = 0.25, linewidth = 0.35) +
    scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
    scale_fill_manual(values = group_colors)

  for (i in seq_along(group_names)) {
    group_name <- group_names[i]
    mean_rate <- rates[group_name]
    color <- group_colors[i]
    p <- p + geom_vline(xintercept = mean_rate, color = color, linetype = "dashed", linewidth = 0.3) +
      annotate("text", x = (mean_rate + 0.0006), y = 50, label = str_c("CR ", group_name, ": ", scales::percent(mean_rate, accuracy = 0.01)),
               color = color, angle = 90, check_overlap = TRUE, size = 5)
  }

  p + labs(x = "", y = "") +
    theme_minimal() +
    theme(
      legend.position = "none",
      axis.text.x = element_text(size = 11, face = "bold"),
      axis.text.y = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank()
    )
}


checkInputs <- function(visitors, conversions) {
  if(any(is.null(visitors), is.null(conversions),
         !is.numeric(visitors), !is.numeric(conversions),
         visitors <= 0, conversions < 0, conversions > visitors)) {
    return(FALSE)
  }
  return(TRUE)
}

# # Testing function
# plot_density_comparison <- function(simulations, rates, group_names, group_colors) {
#   df <- map2_dfr(simulations, names(simulations), ~data.frame(Group = .y, ConversionRate = .x))
#   
#   # Debugging: Print the first few rows of df
#   print(head(df))
#   
#   # Rest of your function...
# }