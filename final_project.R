library(readr)
setwd("/Users/sumukhramagiri/Desktop/Capstone")
Life_expectancy <- read.csv("Life_Expectancy_Data.csv")
missing_values <- is.na(Life_expectancy)
sum(missing_values)
summary(Life_expectancy)


library(ggplot2)
ggplot(Life_expectancy, aes(x = `Life.expectancy`)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black") +
  labs(title = "Histogram of Life Expectancy", x = "Life.expectancy", y = "Frequency")

library(dplyr)
library(ggplot2)
low_gdp <- Life_expectancy %>%
  filter(GDP < 10000)
ggplot(low_gdp, aes(x = GDP, y = `Life.expectancy`)) +
  geom_point(alpha = 0.3, size = 1) +
  geom_smooth(method = "lm", color = "blue") +
  labs(title = "GDP(Countries with low gdp) vs. Life Expectancy", x = "GDP (USD)", y = "Life.expectancy")

high_gdp <- Life_expectancy %>%
  filter(GDP > 10000)

ggplot(high_gdp, aes(x = GDP, y = `Life.expectancy`)) +
  geom_point(alpha = 0.3, size = 1) +
  geom_smooth(method = "lm", color = "blue") +
  labs(title = "GDP(Countries with high gdp) vs. Life Expectancy", x = "GDP (USD)", y = "Life.expectancy")

model_Hepatitis.B <- lm(`Life.expectancy` ~ `Hepatitis.B`, data = Life_expectancy)
summary(model_Hepatitis.B)
library(dplyr)
library(ggplot2)


ggplot(Life_expectancy, aes(x = `Hepatitis.B`, y = `Life.expectancy`)) +
  geom_jitter(alpha = 0.2, size = 1.5, width = 0.3, height = 0) + 
  geom_smooth(method = "lm", color = "blue", fill = "lightblue") + 
  labs(title = "Hepatitis B Vaccination Coverage vs. Life Expectancy",
       x = "Hepatitis B Vaccination Coverage (%)",
       y = "Life Expectancy (Years)") +
  theme_minimal(base_size = 14) +  
  theme(
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 12)
  ) + 
  scale_x_continuous(breaks = seq(0, 100, by = 10)) +  
  scale_y_continuous(breaks = seq(40, 90, by = 5)) 

#Linear model for Polio vaccination
model_polio <- lm(`Life.expectancy` ~ Polio, data = Life_expectancy)
summary(model_polio)
library(dplyr)
library(ggplot2)
filtered_data <- Life_expectancy %>%
  filter(Polio > 40, Polio <= 100, `Life.expectancy` > 40)
#Scatter plot with regression line for Polio and Life Expectancy
ggplot(filtered_data, aes(x = Polio, y = `Life.expectancy`)) +
  geom_point(alpha = 0.3, size = 1) +
  geom_smooth(method = "lm", color = "blue") +
  labs(title = "Polio Vaccination Coverage vs. Life Expectancy",
       x = "Polio Vaccination Coverage (%)",
       y = "Life Expectancy (Years)") +
  theme_minimal() +
  theme(plot.title = element_text(size = 12, face = "bold"),  
        axis.title = element_text(size = 10, face = "bold"),   
        axis.text = element_text(size = 10)) +
  scale_x_continuous(breaks = seq(0, 100, by = 5)) +  
  scale_y_continuous(breaks = seq(40, 90, by = 5)) 

# Linear model for Diphtheria vaccination
model_diphtheria <- lm(`Life.expectancy` ~ Diphtheria, data = Life_expectancy)
summary(model_diphtheria)
# Scatter plot with regression line for Diphtheria and Life Expectancy
filtered_data <- Life_expectancy %>%
  filter(Diphtheria > 40, Diphtheria <= 100, `Life.expectancy` > 40, `Life.expectancy` <= 100)

# Creating the plot with the filtered data
ggplot(filtered_data, aes(x = Diphtheria, y = `Life.expectancy`)) +
  geom_point(alpha = 0.3, size = 1, width = 0.2) +
  geom_smooth(method = "lm", color = "blue", se = TRUE) +
  labs(title = "Diphtheria Vaccination Coverage vs. Life Expectancy",
       x = "Diphtheria Vaccination Coverage (%)",
       y = "Life Expectancy (Years)") +
  theme_minimal() +
  theme(plot.title = element_text(size = 12, face = "bold"),
        axis.title = element_text(size = 10, face = "bold"),
        axis.text = element_text(size = 10)) +
  scale_x_continuous(breaks = seq(0, 100, by = 10)) +  
  scale_y_continuous(breaks = seq(min(filtered_data$`Life.expectancy`), 
                                  max(filtered_data$`Life.expectancy`), by = 5))

# Creating the multiple regression model
economic_model <- lm(Life.expectancy ~ GDP + percentage.expenditure + Income.composition.of.resources, 
                     data = Life_expectancy)
summary(economic_model)


# Making sure that Status is factorized 
Life_expectancy$Status <- as.factor(Life_expectancy$Status)

# Building the multiple regression model with schooling, status and BMI
model <- lm(Life.expectancy ~ Schooling + Status + BMI, data = Life_expectancy)
summary(model)

library(ggplot2)

# Line plot for Schooling vs Life Expectancy with Status distinction
ggplot(Life_expectancy, aes(x = Schooling, y = Life.expectancy, color = Status)) +
  geom_line(alpha = 0.6) +  # Use geom_line for a line plot
  geom_smooth(aes(group = Status), method = "lm", se = TRUE) +
  labs(title = "Impact of Education on Life Expectancy by Status",
       x = "Years of Schooling",
       y = "Life Expectancy") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
    legend.title = element_text(face = "bold"),
    legend.position = "bottom",
    axis.title = element_text(size = 10, face = "bold"),
    axis.text = element_text(size = 10)
  ) +
  scale_color_manual(values = c("Developed" = "blue", "Developing" = "red")) +
  scale_x_continuous( breaks = seq(min(Life_expectancy$Schooling, na.rm = TRUE, by = 1), 
                                   max(Life_expectancy$Schooling, na.rm = TRUE), by = 1),
                      limits = c(0, NA)) +
  scale_y_continuous( breaks = seq(min(Life_expectancy$Life.expectancy, na.rm = TRUE, by = 3), 
                                   max(Life_expectancy$Life.expectancy, na.rm = TRUE), by = 5),
                      limits = c(40, NA))


# Line plot for BMI vs Life Expectancy with Status distinction
ggplot(Life_expectancy, aes(x = BMI, y = Life.expectancy, color = Status)) +
  geom_line(alpha = 0.6) +  # Use geom_line for a line plot
  geom_smooth(aes(group = Status), method = "lm", se = TRUE) +
  labs(title = "Impact of BMI on Life Expectancy by Status",
       x = "BMI",
       y = "Life Expectancy") +
  theme_minimal() +
  theme(plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
        legend.title = element_text(face = "bold"),
        legend.position = "bottom",
        axis.title = element_text(size = 10, face = "bold"),
        axis.text = element_text(size = 10)
  ) +
  scale_color_manual(values = c("Developed" = "blue", "Developing" = "red")) +
  scale_x_continuous(
    breaks = seq(min(Life_expectancy$BMI, na.rm = TRUE), 
                 max(Life_expectancy$BMI, na.rm = TRUE), 
                 by = 5),limits = c(10, NA) 
  ) +
  scale_y_continuous(
    breaks = seq(min(Life_expectancy$Life.expectancy, na.rm = TRUE),                max(Life_expectancy$Life.expectancy, na.rm = TRUE),                by = 5),limits = c(40, NA)
  )

