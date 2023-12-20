
# Libraries
library(shiny)
library(shinydashboard)
library(tidyverse)
library(BayesFactor)
theme_set(theme_minimal())

source("plot_density_function.R")

ui <- fluidPage(
  
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
        column(6, numericInput("visitorsA", "Visitors A", value = 10000,
                               label = span("Visitors A", style = "color: #545454; font-size: 12px;"))),
        column(6, numericInput("conversionsA", "Conversions A", value = 1100,
                               label = span("Conversions A", style = "color: #545454; font-size: 12px;")))
      ),
      fluidRow(
        column(6, numericInput("visitorsB", "Visitors B", value = 10000,
                               label = span("Visitors B", style = "color: #545454; font-size: 12px;"))),
        column(6, numericInput("conversionsB", "Conversions B", value = 1200,
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
      fluidRow(
        column(3, valueBoxOutput("upliftBox"), height = 1),
        column(9, plotOutput("conversionComparisonPlot"), height = 1)
      ),
      plotOutput("densityComparisonPlot")
    )
    
  )
)


# Server Logic
server <- function(input, output) {
  
  # Reactive expression for conf level
  confidence_level_reactive <- reactive({
    # Mapping selected confidence level to its z-score
    conf_level <- as.numeric(input$confidenceLevel)
    z_score <- qnorm((1 + conf_level) / 2)  # Calculating the z-score for the given confidence level
    return(z_score)
  })
  
  simulations_reactive <- reactive({
    
    visitors    <- c(A = input$visitorsA, B = input$visitorsB)
    conversions <- c(A = input$conversionsA, B = input$conversionsB)
    
    # Posterior Distributions
    prior <- c(1, 1)
    post  <- rbind(
      c(conversions["A"] + prior[1], visitors["A"] - conversions["A"] + prior[2]),
      c(conversions["B"] + prior[1], visitors["B"] - conversions["B"] + prior[2])
    )
    
    set.seed(123)
    simulations <- list(
      A = rbeta(100000, post[1, 1], post[1, 2]),
      B = rbeta(100000, post[2, 1], post[2, 2])
    )
    return(simulations)
  })
  
  # Reactive expression for rates
  rates_reactive <- reactive({
    visitors    <- c(A = input$visitorsA, B = input$visitorsB)
    conversions <- c(A = input$conversionsA, B = input$conversionsB)
    rates       <- conversions / visitors
    return(rates)
  })
  
  # Reactive expression for data
  data_reactive <- reactive({
    visitors    <- c(A = input$visitorsA, B = input$visitorsB)
    conversions <- c(A = input$conversionsA, B = input$conversionsB)
    
    # Ensure the input values are appropriate
    if(any(visitors < 0) || any(conversions < 0) || any(conversions > visitors)) {
      return(NULL)  # Return NULL if inputs are not valid
    }
    
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
    
    # Statistics (optional, used if you want to display them somewhere in the app)
    prob_B_better_than_A <- mean(simulations$B > simulations$A)
    rates                <- conversions / visitors
    overall_rate         <- sum(conversions) / sum(visitors)
    standard_errors      <- sqrt(rates * (1 - rates) / visitors)
    
    # Z-Score
    z_score <- (rates["B"] - rates["A"]) / sqrt(sum(standard_errors^2))
    relative_uplift_conversion_rate <- (rates["B"] - rates["A"]) / rates["A"]
    
    z_score <- confidence_level_reactive()
    
    # Compare conversion rates with error bars
    comparison_df <- tibble(
      type          = names(rates),
      rate          = rates,
      std_err       = standard_errors,
      conf_lower    = rates - z_score * standard_errors,
      conf_upper    = rates + z_score * standard_errors
    )
    
    return(
      list(
        comparison_df   = comparison_df,
        relative_uplift = relative_uplift_conversion_rate
        )
      )  # Return the DataFrame for plotting
  })
  
  # Plot Output
  output$conversionComparisonPlot <- renderPlot({
    comparison_df <- data_reactive()$comparison_df  # Fetching the reactive data
    
    if(is.null(comparison_df)) {
      return()  # Do not render the plot if data is NULL
    }
    
    # Ensure 'type' is a factor and handle NA values
    comparison_df$type <- factor(comparison_df$type, levels = unique(comparison_df$type))
    comparison_df <- na.omit(comparison_df)  # Remove rows with NA values
    
    # ggplot code
    ggplot(comparison_df, aes(x = fct_reorder(type, rate, .na_rm = TRUE), y = rate, ymin = conf_lower, ymax = conf_upper, color = type)) +
      geom_crossbar(position = position_dodge(0.5), width = 0.32, size = 0.35) +
      geom_text(aes(label = scales::percent(rate, accuracy = 0.01)), nudge_x =  0.25, size = 4, check_overlap = TRUE) +
      scale_y_continuous(labels = scales::percent) +
      scale_color_manual(values = c("A" = "#2E465F", "B" = "#D81B60")) +
      labs(
        title = NULL,
        x     = "", 
        y     = NULL
      ) +
      coord_flip() +
      theme(
        legend.position = "none",
        panel.grid      = element_blank(),
        axis.text.x     = element_blank(),
        plot.background = element_rect(fill = "#F5F5F5", color = NA),
        axis.text       = element_text(color = "#545454", size = 11, face = "bold")
      )
  }, height = 250)
  
  output$densityComparisonPlot <- renderPlot({
    # Ensure the simulations data is available
    simulations <- simulations_reactive()  # Replace with your actual reactive expression for simulations
    rates       <- rates_reactive()  # Replace with your actual reactive expression for rates
    
    if(is.null(simulations) || is.null(rates)) {
      return()  # Do not render the plot if data is NULL
    }
    
    # Define group names and colors (adjust as needed)
    group_names <- c("A", "B")
    group_colors <- c("A" = "#2E465F", "B" = "#D81B60")
    
    # Call the plot_density_comparison function
    plot_density_comparison(simulations, rates, group_names, group_colors)
  })
  
  relative_uplift_reactive <- reactive({
    data <- data_reactive()
    if (is.null(data)) {
      return(NULL)
    }
    return(data$relative_uplift)
  })
  
  output$upliftBox <- renderValueBox({
    uplift <- relative_uplift_reactive()
    if (is.null(uplift)) {
      valueBox("N/A", "Relative Uplift", icon = NULL, color = "gray")
    } else {
      valueBox(scales::percent(uplift, accuracy = 0.01), "Relative Uplift in CR", icon = NULL)
    }
  })
  
}

# Run the App
shinyApp(ui = ui, server = server)

