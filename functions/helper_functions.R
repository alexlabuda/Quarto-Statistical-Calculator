
## Outlier winsor----
outlier_winsorize <- function(data, column_to_winsor,
                              multiplier = 1.5) {
  
  if (!column_to_winsor %in% names(data)) {
    stop(paste("Column", column_to_winsor, "not found in the data frame."))
  }
  
  data_clean <- data |> 
    mutate(
      iqr         = IQR(.data[[column_to_winsor]], na.rm = TRUE),
      quant_75    = quantile(.data[[column_to_winsor]], probs = 0.75, na.rm = TRUE),
      quant_25    = quantile(.data[[column_to_winsor]], probs = 0.25, na.rm = TRUE),
      upper_fence = quant_75 + multiplier * iqr,
      lower_fence = quant_25 - multiplier * iqr
    ) |> 
    mutate(
      !!column_to_winsor := case_when(
        .data[[column_to_winsor]] > upper_fence ~ upper_fence,
        .data[[column_to_winsor]] < 0           ~ 0,
        .default = .data[[column_to_winsor]]
      )) |> 
    select(-iqr, -quant_25, -quant_75, -lower_fence)
  return(data_clean)
}


## Outlier removal----

# outlier_removal <- function(data, column_to_handle,
#                               multiplier = 1.5) {
#   
#   if (!column_to_handle %in% names(data)) {
#     stop(paste("Column", column_to_handle, "not found in the data frame."))
#   }
#   
#   data_clean <- data |> 
#     mutate(
#       iqr         = IQR(.data[[column_to_handle]], na.rm = TRUE),
#       quant_75    = quantile(.data[[column_to_handle]], probs = 0.75, na.rm = TRUE),
#       quant_25    = quantile(.data[[column_to_handle]], probs = 0.25, na.rm = TRUE),
#       upper_fence = quant_75 + multiplier * iqr,
#       lower_fence = quant_25 - multiplier * iqr
#     ) |> 
#     mutate(
#       outlier_flag = case_when(
#         .data[[column_to_handle]] > upper_fence ~ "outlier",
#         .data[[column_to_handle]] < lower_fence ~ "outlier",
#         .default = "not_outlier"
#       )) |> 
#     filter(outlier_flag == "not_outlier") |> 
#     select(-iqr, -quant_25, -quant_75, -upper_fence, -lower_fence, -outlier_flag)
#     
#   return(data_clean)
# }

