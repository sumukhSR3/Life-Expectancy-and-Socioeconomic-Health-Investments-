library(readr)
Life_Expectancy <- read_csv("Desktop/Capstone/Life_Expectancy_Data.csv")
View(Life_Expectancy)
str(Life_Expectancy)


library(dplyr)
library(tidyr)
library(ggplot2)

#data cleaning

Life_Expectancy <- Life_Expectancy %>%
  mutate(across(where(is.numeric), ~ifelse(is.na(.), median(., na.rm = TRUE), .))) 
# Inspecting the dataset
# by viewing the first few rows of the dataset to get a quick glimpse of the data.
head(Life_Expectancy)

#Using the summary() function to get summary statistics of the dataset. 
#As it is useful for quickly checking the range, median, and presence of missing values in your numeric columns after cleaning.
summary(Life_Expectancy)


#In many cases we observe that there are missing values in a dataset. To check the data if there are any missing values we use the following command.

sum(is.na(Life_Expectancy))

#Visualizing the data can help you spot any anomalies or issues that weren't apparent during the cleaning process. 
#And by plotting histograms of numeric variables can help us understand their distributions and identifying any outliers.

# Histogram of life expectancy
ggplot(Life_Expectancy, aes(x = `Life expectancy`)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black") +
  labs(title = "Histogram of Life Expectancy", x = "Life Expectancy", y = "Frequency")

#from the histogram we observed that the data is mostly concentrated from the year 70 to 80.
#But there is a possibility of outliers in the begining years of the data.


#for identifying the outliers let's use the interquartile range (IQR)

# Calculate the IQR for Life expectancy
Q1 <- quantile(Life_Expectancy$`Life expectancy`, 0.25, na.rm = TRUE)
Q3 <- quantile(Life_Expectancy$`Life expectancy`, 0.75, na.rm = TRUE)
IQR <- Q3 - Q1

# Define the lower and upper bounds for outliers
lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR

# Identify outliers
outliers <- subset(Life_Expectancy, `Life expectancy` < lower_bound | `Life expectancy` > upper_bound)


# View outliers
print(outliers)
#Based on the values of the outliers, it appears that the outlier life expectancy values are mainly concentrated in the lower range (around 44.5 to 48.2 years), which is significantly below the global average.

#Given the countries and the years mentioned (e.g., Angola, Botswana in the early 2000s), these low life expectancy figures could be genuine due to several factors:
  
#HIV/AIDS Epidemic: The early 2000s were marked by a severe HIV/AIDS epidemic in Sub-Saharan Africa, which drastically reduced life expectancy in affected countries. The high HIV/AIDS values in the outliers support this.
#Healthcare System: Developing countries often have healthcare systems that struggle with the burden of infectious diseases, malnutrition, and other health issues, impacting life expectancy.

#As the outliers are genuine we can proceed with the further analysis of the dataset.

#Global Trend in Life Expectancy Over the Years
global_trend <- Life_Expectancy %>%
  group_by(Year) %>%
  summarize(Average_Life_Expectancy = mean(`Life expectancy`, na.rm = TRUE))

ggplot(global_trend, aes(x = Year, y = Average_Life_Expectancy)) +
  geom_line() +
  geom_point() +
  labs(title = "Global Life Expectancy Over Years", x = "Year", y = "Average Life Expectancy") +
  theme_minimal()
#Insight: As the trend shows a global increase in life expectancy, it suggests overall improvements in health outcomes. However, the rate of increase or any plateaus might indicate emerging challenges or diminishing returns on current healthcare strategies.
#Investment Focus: Countries contributing to a slower rate of increase may need investment in healthcare infrastructure, technologies, and services to address specific health challenges.

#Life Expectancy Variance Between Countries and Regions

ggplot(Life_Expectancy, aes(x = Country, y = `Life expectancy`)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(title = "Life Expectancy Variance Between Countries", x = "Country", y = "Life Expectancy")
#Insight: Significant variance between countries highlights disparities in health outcomes. Countries with lower life expectancy or high variance within the country suggest inequalities in access to healthcare services, quality of care, or public health challenges.
#Investment Focus: Countries with lower median life expectancy or wider interquartile ranges present opportunities for impactful investment. These regions may benefit from investments in basic healthcare infrastructure, access to essential medicines, vaccination programs, and public health initiatives.



#Health-Related Factors Correlation with Life Expectancy
health_factors <- Life_Expectancy %>%
  select(`Life expectancy`, `Adult Mortality`, `infant deaths`, `Alcohol`, `Hepatitis B`, `Measles`, `BMI`, `under-five deaths`, `Polio`, `Diphtheria`, `HIV/AIDS`)

correlation_matrix <- cor(health_factors, use = "complete.obs")
corrplot::corrplot(correlation_matrix, method = "circle")
#Insight: Correlations between life expectancy and health-related factors (e.g., adult mortality, HIV/AIDS prevalence) highlight specific health issues that significantly impact longevity.
#Investment Focus: Investing in countries with strong negative correlations between life expectancy and health challenges (like high adult mortality or high HIV/AIDS rates) can be strategic. Focus on comprehensive healthcare services, disease prevention programs, and education can address these specific challenges.




#Efficiency of Healthcare Expenditure

ggplot(Life_Expectancy, aes(x = `percentage expenditure`, y = `Life expectancy`)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title = "Healthcare Expenditure vs. Life Expectancy", x = "Healthcare Expenditure (%)", y = "Life Expectancy") +
  theme_minimal()

#Insight: The relationship between healthcare expenditure and life expectancy can indicate the efficiency of healthcare spending. A positive correlation suggests that investments in healthcare lead to better health outcomes.
#Investment Focus: Countries with low life expectancy despite high healthcare expenditure may need investments aimed at increasing the efficiency of healthcare spending, such as through improving healthcare delivery systems, enhancing healthcare worker training, or investing in preventive healthcare.


#Impact of Lifestyle Factors

ggplot(Life_Expectancy, aes(x = Alcohol, y = `Life expectancy`)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title = "Alcohol Consumption vs. Life Expectancy", x = "Alcohol Consumption (per capita)", y = "Life Expectancy")

ggplot(Life_Expectancy, aes(x = BMI, y = `Life expectancy`)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title = "BMI vs. Life Expectancy", x = "BMI", y = "Life Expectancy")


#Insight: Lifestyle factors like alcohol consumption and BMI affecting life expectancy point to the importance of preventive healthcare and public health initiatives.
#Investment Focus: Countries where lifestyle factors significantly impact life expectancy could benefit from investments in public health campaigns, lifestyle modification programs, and preventive healthcare services.



#Predicting Life Expectancy
model <- lm(`Life expectancy` ~ `Adult Mortality` + BMI + GDP + Schooling + `HIV/AIDS`, data = Life_Expectancy)
summary(model)

#Insight:
#Significant Predictors: The regression model indicates that adult mortality, BMI, GDP, schooling, and HIV/AIDS prevalence are significant predictors of life expectancy. This is evident from their low p-values (p < 0.05) and the confidence intervals not containing zero.
#Direction of Influence: Adult mortality and HIV/AIDS prevalence have negative coefficients, indicating that higher levels of these factors are associated with lower life expectancy. Conversely, BMI, GDP, and schooling have positive coefficients, suggesting that higher levels of these factors are associated with higher life expectancy.
#Model Performance: The multiple R-squared value of 0.8033 indicates that approximately 80.33% of the variability in life expectancy can be explained by the combination of these predictors.
#Investment Focus:
#Addressing Adult Mortality and HIV/AIDS Prevalence:
#Insight: Adult mortality and HIV/AIDS have strong negative associations with life expectancy. Investments should focus on strategies to reduce mortality rates and combat the spread of HIV/AIDS.
#Investment Focus: Healthcare organizations can invest in improving healthcare infrastructure, access to essential medications, and implementing prevention and treatment programs targeted at reducing adult mortality and controlling HIV/AIDS transmission.
#Improving Socioeconomic Factors:
#Insight: BMI, GDP, and schooling have positive associations with life expectancy. This suggests that improving socioeconomic factors can positively impact life expectancy.
#Investment Focus: Investments should be directed towards initiatives aimed at improving education, economic development, and addressing poverty. This could include investing in education systems, vocational training programs, economic empowerment initiatives, and poverty alleviation measures.
#Promoting Healthy Lifestyles:
#Insight: BMI, a measure of body mass index, is positively associated with life expectancy. This highlights the importance of promoting healthy lifestyles and reducing obesity rates.
#Investment Focus: Healthcare organizations can invest in public health campaigns, nutritional education programs, and initiatives aimed at promoting physical activity and healthy eating habits. Investing in facilities and programs that encourage physical activity and access to healthy food options can also be beneficial.
#Supporting Economic Development:
#Insight: GDP is positively associated with life expectancy, indicating that economic development plays a crucial role in improving health outcomes.
#Investment Focus: Investments in economic development projects, infrastructure development, and job creation initiatives can indirectly contribute to improvements in health outcomes by lifting people out of poverty, improving living standards, and increasing access to healthcare services.
#Enhancing Healthcare Infrastructure and Access:
#Insight: While not directly included in the model, investments in healthcare infrastructure, access to healthcare services, and quality healthcare delivery are essential for improving overall health outcomes.
#Investment Focus: Healthcare organizations can invest in building and upgrading healthcare facilities, training healthcare workers, expanding access to essential medicines, and implementing telemedicine and mobile health solutions to reach underserved populations.



