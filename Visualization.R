library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)

Life_expectancy <- read.csv("Life_Expectancy_Data.csv")

# Creating the user interface
ui <- dashboardPage(
  dashboardHeader(title = "Life Expectancy Dashboard"),
  dashboardSidebar(
    selectInput("selectedCountry", "Select Country", choices = sort(unique(Life_expectancy$Country))),
    sliderInput("selectedYear", "Select Year Range", min = min(Life_expectancy$Year),
                max = max(Life_expectancy$Year), value = c(min(Life_expectancy$Year), max(Life_expectancy$Year)))
  ),
  dashboardBody(
    fluidRow(
      box(plotOutput("alcoholPlot"), title = "Alcohol Consumption vs Life Expectancy", width = 10),
      box(plotOutput("healthExpenditurePlot"), title = "Health Expenditure vs Infant Deaths", width = 10)
    ),
    fluidRow(
      box(plotOutput("hepBPlot"), title = "Hepatitis B vs Life Expectancy", width = 10),
      box(plotOutput("polioPlot"), title = "Polio vs Life Expectancy", width = 10),
      box(plotOutput("diphtheriaPlot"), title = "Diphtheria vs Life Expectancy", width = 10)
    ),
    fluidRow(
      box(plotOutput("gdpPlot"), title = "GDP vs Life Expectancy", width = 10)
    )
  )
)

# Assigning the server logic to display plots required.
server <- function(input, output) {
  filtered_data <- reactive({
    Life_expectancy %>%
      filter(Country == input$selectedCountry, 
             Year >= input$selectedYear[1], 
             Year <= input$selectedYear[2])
  })
  
  output$alcoholPlot <- renderPlot({
    ggplot(filtered_data(), aes(x = Alcohol, y = Life.expectancy)) +
      geom_point(aes(color = Life.expectancy)) +
      geom_smooth(method = "lm", se = TRUE, color = "blue") +
      labs(x = "Alcohol Consumption (litres per capita)", y = "Life Expectancy (years)")
  })
  
  output$healthExpenditurePlot <- renderPlot({
    ggplot(filtered_data(), aes(x = percentage.expenditure, y = infant.deaths)) +
      geom_point(aes(color = infant.deaths)) +
      geom_smooth(method = "lm", se = TRUE, color = "red") +
      labs(x = "Health Expenditure (% of GDP)", y = "Infant Deaths")
  })
  
  output$hepBPlot <- renderPlot({
    ggplot(filtered_data(), aes(x = Hepatitis.B, y = Life.expectancy)) +
      geom_point(aes(color = Life.expectancy)) +
      geom_smooth(method = "lm", se = TRUE, color = "green") +
      labs(x = "Hepatitis B Vaccination Coverage (%)", y = "Life Expectancy (years)")
  })
  
  output$polioPlot <- renderPlot({
    ggplot(filtered_data(), aes(x = Polio, y = Life.expectancy)) +
      geom_point(aes(color = Life.expectancy)) +
      geom_smooth(method = "lm", se = TRUE, color = "orange") +
      labs(x = "Polio Vaccination Coverage (%)", y = "Life Expectancy (years)")
  })
  
  output$diphtheriaPlot <- renderPlot({
    ggplot(filtered_data(), aes(x = Diphtheria, y = Life.expectancy)) +
      geom_point(aes(color = Life.expectancy)) +
      geom_smooth(method = "lm", se = TRUE, color = "purple") +
      labs(x = "Diphtheria Vaccination Coverage (%)", y = "Life Expectancy (years)")
  })
  
  output$gdpPlot <- renderPlot({
    ggplot(filtered_data(), aes(x = GDP, y = Life.expectancy)) +
      geom_point(aes(color = Life.expectancy)) +
      geom_smooth(method = "lm", se = TRUE, color = "yellow") +
      labs(x = "GDP", y = "Life Expectancy (years)")
  })
}

# Run the Shiny app.
shinyApp(ui, server)
