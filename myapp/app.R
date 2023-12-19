#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/

# Read in data we will use:
# df <- read_csv("test/data/test_transactions_data.csv")


library(shiny)
library(tidyverse)

# Define UI for application that draws a histogram
ui <- fluidPage(# Application title
  titlePanel("Booking"),
  
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      sliderInput(
        "bins",
        "Number of bins:",
        min = 1,
        max = 50,
        value = 30
      ),
      # add file input section
      fileInput(
        "file",
        "Choose CSV File",
        multiple = TRUE,
        accept = c("text/csv",
                   "text/comma-separated-values,text/plain",
                   ".csv")
      )
    ),
    
    # Show a plot of the generated distribution
    mainPanel(plotOutput("distPlot"))
  ))

# Define server logic required to draw a histogram
server <- function(input, output) {
  raw <- reactive({
    req(input$file)
    vroom::vroom(
      input$file$datapath,
      .name_repair = janitor::make_clean_names
    ) |> 
      mutate(transaction_revenue = transaction_revenue / 1000000)
  })
  
  
  
  output$distPlot <- renderPlot({
    # generate bins based on input$bins from ui.R
    x    <- faithful[, 2]
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    # draw the histogram with the specified number of bins
    hist(
      x,
      breaks = bins,
      col = 'darkgray',
      border = 'white',
      xlab = 'Waiting time to next eruption (in mins)',
      main = 'Histogram of waiting times'
    )
  })
  
  output$boxPlot <- renderPlot({
    raw |> 
      ggplot(aes(x = year, y = price))
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)
