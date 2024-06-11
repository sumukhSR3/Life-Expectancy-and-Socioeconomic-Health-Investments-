# Install Shiny package
install.packages("shiny")

# Load Shiny library
library(shiny)

# Define UI
ui <- fluidPage(
  sliderInput("num", "Choose a number", 1, 100, 50),
  plotOutput("distPlot")
)

# Define server logic
server <- function(input, output) {
  output$distPlot <- renderPlot({
    hist(rnorm(input$num))
  })
}

# Run the app
shinyApp(ui = ui, server = server)




library(ggplot2)
library(dplyr)
library(readr)
Life_Expectancy_Data <- read_csv("Life_Expectancy_Data.csv")
#View(Life_Expectancy_Data)

ui <- fluidPage(
  titlePanel("Global Health Insights on Life Expectancy"),
  sidebarLayout(
    sidebarPanel(
      selectInput("countryInput", "Select Country:", 
                  choices = unique(Life_Expectancy_Data$Country)),
      sliderInput("yearRange", "Select Year Range:",
                  min = min(Life_Expectancy_Data$Year),
                  max = max(Life_Expectancy_Data$Year),
                  value = c(2000, 2015),
                  step = 1)
    ),
    mainPanel(
      plotOutput("lifeExpPlot"),
      plotOutput("healthExpPlot")
    )
  )
)
#Interactive Plot 

# Define UI
ui <- fluidPage(
  selectInput("countryInput", "Select Country:", choices = unique(Life_Expectancy_Data$Country)),
  sliderInput("yearRange", "Select Year Range:",
              min = min(Life_Expectancy_Data$Year),
              max = max(Life_Expectancy_Data$Year),
              value = c(2000, 2015)),
  plotOutput("trendPlot")
)

# Define server logic
server <- function(input, output) {
  
  output$trendPlot <- renderPlot({
    req(input$countryInput) # Ensure that a country is selected
    
    filtered_data <- Life_Expectancy_Data %>%
      filter(Country == input$countryInput, 
             Year >= input$yearRange[1], 
             Year <= input$yearRange[2])
    
    ggplot(filtered_data, aes(x = Year, y = `Life expectancy`)) +
      geom_line() + geom_point() +
      labs(title = "Life Expectancy Over Time", x = "Year", y = "Life Expectancy") +
      theme_minimal()
  })
  
}
#Health Indicators Comparison
ui <- fluidPage(
  titlePanel("Health Indicators Comparison"),
  sidebarLayout(
    sidebarPanel(
      selectInput("countryInput", "Select Countries:", choices = unique(Life_Expectancy_Data$Country), multiple = TRUE),
      selectInput("indicatorInput", "Select Health Indicators:", choices = c("Adult Mortality", "BMI", "GDP"), multiple = TRUE),
      actionButton("compareBtn", "Compare")
    ),
    mainPanel(
      plotOutput("indicatorComparisonPlot")
    )
  )
)

server <- function(input, output) {
  observeEvent(input$compareBtn, { # React to the compare button click
    output$indicatorComparisonPlot <- renderPlot({
      req(length(input$countryInput) > 0) # Ensure at least one country is selected
      req(length(input$indicatorInput) > 0) # Ensure at least one indicator is selected
      
      # Filter data based on selected countries
      filtered_data <- Life_Expectancy_Data %>% 
        filter(Country %in% input$countryInput) %>%
        select(Country, Year, all_of(input$indicatorInput)) # Dynamically select indicators
      
      # Melt the data for ggplot2
      long_data <- tidyr::pivot_longer(filtered_data, cols = -c(Country, Year), names_to = "Indicator", values_to = "Value")
      
      ggplot(long_data, aes(x = Year, y = Value, color = Country)) +
        geom_line() +
        facet_wrap(~Indicator, scales = "free_y") + # Create separate plots for each indicator
        labs(title = "Health Indicators Comparison", x = "Year", y = "Value") +
        theme_minimal()
    })
  })
}



shinyApp(ui = ui, server = server)
