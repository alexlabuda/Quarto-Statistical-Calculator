## Outlier handling
outlier_winsorize <- function(data, column_to_winsor,
                              multiplier = 1.5) {
  
  if (!column_to_winsor %in% names(data)) {
    stop(paste("Column", column_to_winsor, "not found in the data frame."))
  }
  
  data_clean <- data %>%
    mutate(
      iqr         = IQR(.data[[column_to_winsor]], na.rm = TRUE),
      quant_75    = quantile(.data[[column_to_winsor]], probs = 0.75, na.rm = TRUE),
      quant_25    = quantile(.data[[column_to_winsor]], probs = 0.25, na.rm = TRUE),
      upper_fence = quant_75 + multiplier * iqr,
      lower_fence = quant_25 - multiplier * iqr
    ) %>%
    mutate(
      !!column_to_winsor := case_when(
        .data[[column_to_winsor]] > upper_fence ~ upper_fence,
        .data[[column_to_winsor]] < lower_fence ~ lower_fence,
        .default = .data[[column_to_winsor]]
      )) %>%
    select(-iqr, -quant_25, -quant_75, -upper_fence, -lower_fence)
  return(data_clean)
}