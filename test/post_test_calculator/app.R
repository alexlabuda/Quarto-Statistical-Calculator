
library(shiny)

# ui <- fluidPage(
#   titlePanel("Your Shiny App"),
#   sidebarLayout(
#     sidebarPanel(
#       # First set of parameters: Numeric Inputs
#       numericInput("visitorsA", "Visitors A", value = 0),
#       numericInput("visitorsB", "Visitors B", value = 0),
#       numericInput("conversionsA", "Conversions A", value = 0),
#       numericInput("conversionsB", "Conversions B", value = 0),
#       
#       # Second set of parameters: Radio Buttons for One-sided or Two-sided
#       radioButtons("testType", "Hypothesis",
#                    choices = c("One-sided", "Two-sided"),
#                    selected = "One-sided"),
#       
#       # Third set of parameters: Radio Buttons for Confidence Level
#       radioButtons("confidenceLevel", "Confidence",
#                    choices = c("90%" = 0.90, "95%" = 0.95, "99%" = 0.99),
#                    selected = 0.95)
#     ),
#     mainPanel(
#       # Placeholder for outputs
#     )
#   )
# )

library(shiny)

ui <- fluidPage(
  
  # titlePanel("AB Test Evaluation"),
  
  # div(class = "titlePanel", 
  #     h1("AB Test Evaluation", style = "color: black; font-size: 28px; text-align: left;")),
  
  div(class = "header", style = "display: flex; align-items: center;",
      img(src = "ZZ-logo_Z-only.png", height = "110px", style = "margin-right: 10px;"),
      div(class = "titlePanel", 
          h1("AB Test Evaluation", style = "margin: 0; color: #333232; font-size: 46px;")
      )
  ),
  
  sidebarLayout(
    sidebarPanel(
      h4("Is your test result significant? Does it have enough power?", style = "font-size: 20px"),
      h5("Play with the controls and get a better feel for how a lower confidence level will boost the power or how an increase in test size can make a small conversion rate difference significant."),
      br(),
      # Use fluidRow and columns to position inputs side by side
      h4("Test data", style = "border-bottom: 1px solid #cccccc; padding-bottom: 5px; font-size: 14px"),
      fluidRow(
        column(6, numericInput("visitorsA", "Visitors A", value = 0,
                               label = span("Visitors A", style = "color: #545454; font-size: 12px;"))),
        column(6, numericInput("conversionsA", "Conversions A", value = 0,
                               label = span("Conversions A", style = "color: #545454; font-size: 12px;")))
      ),
      fluidRow(
        column(6, numericInput("visitorsB", "Visitors B", value = 0,
                               label = span("Visitors B", style = "color: #545454; font-size: 12px;"))),
        column(6, numericInput("conversionsB", "Conversions B", value = 0,
                               label = span("Conversions B", style = "color: #545454; font-size: 12px;")))
      ),
      h4("Settings", style = "border-bottom: 1px solid #cccccc; padding-bottom: 5px; font-size: 14px"),
      # Radio buttons for One-sided or Two-sided
      radioButtons("testType", HTML('<span style="color: #545454; font-size: 12px;">Hypothesis</span>'),
                   choices = c("One-sided", "Two-sided"),
                   selected = "One-sided"),
      
      # Radio buttons for Confidence Level
      radioButtons("confidenceLevel", HTML('<span style="color: #545454; font-size: 12px;">Confidence Level</span>'),
                   choices = c("90%" = 0.90, "95%" = 0.95, "99%" = 0.99),
                   selected = 0.95)
    ),
    mainPanel(
      # Placeholder for outputs
    )
  )
)


server <- function(input, output) {
  # We will fill this in based on your output requirements
}

shinyApp(ui = ui, server = server)
