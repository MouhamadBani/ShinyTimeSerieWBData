# R Shiny App for World Bank Data Analysis

library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(tidyr)
library(readr)

# Read the CSV file
wb_data <- read_csv("C:\\Users\\Mouhamad Bani\\Downloads\\WBData.csv")



# UI
ui <- dashboardPage(
  dashboardHeader(title = "World Bank Data Explorer"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Data Overview", tabName = "overview", icon = icon("dashboard")),
      menuItem("Country Comparison", tabName = "country_comp", icon = icon("chart-bar")),
      menuItem("Time Series", tabName = "time_series", icon = icon("line-chart")),
      menuItem("Correlation Analysis", tabName = "correlation", icon = icon("project-diagram"))
    )
  ),
  
  dashboardBody(
    tabItems(
      # Overview Tab
      tabItem(tabName = "overview",
              fluidRow(
                box(title = "Data Summary", status = "primary", solidHeader = TRUE,
                    width = 12,
                    valueBoxOutput("total_countries"),
                    valueBoxOutput("year_range"),
                    valueBoxOutput("total_indicators")
                )
              ),
              fluidRow(
                box(title = "Indicator Distribution", status = "warning", 
                    plotOutput("indicator_dist_plot"),
                    width = 12
                )
              )
      ),
      
      # Country Comparison Tab
      tabItem(tabName = "country_comp",
              fluidRow(
                box(title = "Country Selector", status = "primary",
                    selectInput("countries", "Select Countries:", 
                                multiple = TRUE,
                                choices = unique(wb_data$`Country Name`)),
                    selectInput("indicator", "Select Indicator:",
                                choices = names(wb_data)[sapply(wb_data, is.numeric)])
                ),
                box(title = "Comparison Plot", status = "success",
                    plotOutput("country_comparison_plot")
                )
              )
      ),
      
      # Time Series Tab
      tabItem(tabName = "time_series",
              fluidRow(
                box(title = "Time Series Analysis", status = "primary",
                    selectInput("time_series_indicator", "Select Indicator:",
                                choices = names(wb_data)[sapply(wb_data, is.numeric)]),
                    checkboxInput("log_scale", "Log Scale", value = FALSE)
                ),
                box(title = "Time Series Plot", status = "success",
                    plotOutput("time_series_plot")
                )
              )
      ),
      
      # Correlation Tab
      tabItem(tabName = "correlation",
              fluidRow(
                box(title = "Correlation Matrix", status = "primary",
                    plotOutput("correlation_matrix"),
                    width = 12
                )
              )
      )
    )
  )
)

# Server
server <- function(input, output) {
  # Overview Outputs
  output$total_countries <- renderValueBox({
    valueBox(
      length(unique(wb_data$`Country Name`)), 
      "Total Countries", 
      icon = icon("globe")
    )
  })
  
  output$year_range <- renderValueBox({
    valueBox(
      paste(min(wb_data$year), "-", max(wb_data$year)),
      "Year Range",
      icon = icon("calendar")
    )
  })
  
  output$total_indicators <- renderValueBox({
    valueBox(
      ncol(wb_data) - 3,  # Subtract Country Name, Country Code, and year
      "Total Indicators", 
      icon = icon("chart-line")
    )
  })
  
  # Indicator Distribution Plot
  output$indicator_dist_plot <- renderPlot({
    # Select a few key numeric indicators
    key_indicators <- names(wb_data)[sapply(wb_data, is.numeric)][1:5]
    
    wb_data %>%
      select(all_of(key_indicators)) %>%
      pivot_longer(everything(), names_to = "Indicator", values_to = "Value") %>%
      ggplot(aes(x = Value)) +
      geom_histogram(bins = 30, fill = "skyblue", color = "black") +
      facet_wrap(~Indicator, scales = "free") +
      theme_minimal() +
      labs(title = "Distribution of Key Indicators")
  })
  
  # Country Comparison Plot
  output$country_comparison_plot <- renderPlot({
    req(input$countries, input$indicator)
    
    wb_data %>%
      filter(`Country Name` %in% input$countries) %>%
      ggplot(aes(x = year, y = !!sym(input$indicator), color = `Country Name`)) +
      geom_line() +
      theme_minimal() +
      labs(
        title = paste("Comparison of", input$indicator),
        x = "Year",
        y = input$indicator
      )
  })
  
  # Time Series Plot
  output$time_series_plot <- renderPlot({
    req(input$time_series_indicator)
    
    p <- wb_data %>%
      group_by(year) %>%
      summarize(mean_value = mean(!!sym(input$time_series_indicator), na.rm = TRUE)) %>%
      ggplot(aes(x = year, y = mean_value)) +
      geom_line(color = "darkblue") +
      theme_minimal() +
      labs(
        title = paste("Global Trend of", input$time_series_indicator),
        x = "Year",
        y = "Mean Value"
      )
    
    if(input$log_scale) {
      p <- p + scale_y_log10()
    }
    
    p
  })
  
  # Correlation Matrix
  output$correlation_matrix <- renderPlot({
    numeric_cols <- sapply(wb_data, is.numeric)
    cor_matrix <- cor(wb_data[, numeric_cols], use = "complete.obs")
    
    cor_matrix %>%
      as.data.frame() %>%
      rownames_to_column("var1") %>%
      pivot_longer(cols = -var1, names_to = "var2", values_to = "correlation") %>%
      ggplot(aes(x = var1, y = var2, fill = correlation)) +
      geom_tile() +
      scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
      labs(title = "Correlation Matrix of Indicators")
  })
}

# Run the app
shinyApp(ui, server)