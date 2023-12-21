library(quarto)

# The name you would like to give the output file
# client name, date, etc.
document_name <- "post-ab-test-automation-test"

# Define the path to your Quarto document
quarto_file_path <- "01-statistical-test-report-automation.qmd"

# Define the output directory
output_dir <- "output_quarto_documents"

# Define new parameter values here:
new_params <- list(
  sample_size_A    = 9000,
  success_A        = 1350,
  sample_size_B    = 9000,
  success_B        = 1450,
  hypothesis       = "two.sided",
  confidence_level = 0.95
)

# Renders the document and saves to directory specified above
quarto_render(
  input         = quarto_file_path,
  output_format = "html",
  output_dir    = output_dir,
  output_file   = paste0(document_name, ".html"),
)