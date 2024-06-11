library(readr)
setwd("/Users/sumukhramagiri/Desktop/Capstone")
Life_expectancy <- read.csv("Life_Expectancy_Data.csv")
summary(Life_expectancy)

#

missing_values <- is.na(Life_expectancy)
sum(missing_values)

#Histogram for Life Expectancy

library(ggplot2)
ggplot(Life_expectancy, aes(x = `Life.expectancy`)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black") +
  labs(title = "Histogram of Life Expectancy", x = "Life.expectancy", y = "Frequency")

#Insights from the histogram we observe the following:

#Central Life Expectancy Range: The most common life expectancy values are clustered around the 70 to 80 years range.
#This suggests that for many countries in the dataset, the average individual lives into their early seventies to late eighties.

#Data Skewness: There appears to be a slight left skew in the data, with a tail extending towards lower life expectancy values. 
#This indicates that there are fewer countries with very low life expectancy, but those that do exist could significantly influence the overall analysis and may represent areas in critical need of health interventions.

#Potential Outliers: The presence of bars at the extreme ends of the life expectancy spectrum suggests there could be outliers. 
#Countries with life expectancies much lower or higher than the global average could be examined to understand the factors contributing to these extremes.

#Global Health Trends: The distribution indicates a general improvement in life expectancy as the bulk of the data is skewed towards the higher end. 
#It reflects global health trends over the years covered by the dataset, likely showing advancements in medical technology, healthcare availability, and public health initiatives.

#Variability Between Countries: The spread of the data suggests there's significant variability in life expectancy between different countries or regions. 
#Which could correlate with differences in economic development, healthcare systems, education, and lifestyle factors among these countries.

#Indication of Data Quality: The relatively smooth distribution with no unexpected spikes suggests that the data is of good quality with no obvious errors in recording life expectancy.

#Basis for Further Statistical Analysis: Given the distribution appears to be approximately bell-shaped, albeit with a slight skew, 
#most parametric statistical analyses that assume normality could be considered for further exploration, with a caveat for the skewness which might require some transformations or non-parametric methods.

#Scatter Plot for GDP vs. Life Expectancy

ggplot(Life_expectancy, aes(x = GDP, y = `Life.expectancy`)) +
  geom_point() +
  geom_smooth(method = "lm", color = "blue") +
  labs(title = "GDP vs. Life Expectancy", x = "GDP (USD)", y = "Life.expectancy")

#Insights from the Scatter Plot

#Positive Correlation: There is a positive correlation between GDP and life expectancy, which suggests that on average, countries with higher GDP per capita tend to have higher life expectancy. 
#This could imply that economic wealth is associated with better access to healthcare, nutrition, and living conditions that contribute to longer lives.

#Diminishing Returns: The slope of the line seems to flatten as GDP increases, indicating diminishing returns; beyond a certain point, increases in GDP have a smaller impact on life expectancy. 
#This could suggest that once a country achieves sufficient economic development to provide for basic health and wellness needs, further economic growth does not significantly increase life expectancy.

#Variability at Lower GDP Levels: There is considerable variability in life expectancy among countries with lower GDP. This spread could reflect differences in how effectively countries use their limited resources 
#The impact of infectious diseases, or other health crises that disproportionately affect poorer nations.

#Data Density: Most of the data points are clustered at the lower end of the GDP scale, indicating that more countries in the dataset have lower GDPs. 
#This density could suggest a need to focus on health strategies in lower-income countries.

#Outliers: There are a few outliers, particularly countries with high GDP but not the highest life expectancy, and vice versa. These might be cases where other factors significantly affect life expectancy beyond economic measures.

#Policy Implications: The positive correlation between GDP and life expectancy supports policies that aim to improve economic conditions as a means to enhance public health. 
#However, due to diminishing returns, high-income countries might need to focus on non-economic factors to further increase life expectancy.


#Now, let's check the relationship between life expectancy and vaccination rates using linear regression:

# Linear model for Hepatitis B vaccination
model_Hepatitis.B <- lm(`Life.expectancy` ~ `Hepatitis.B`, data = Life_expectancy)
summary(model_Hepatitis.B)

#Insights from the linear model Hepatitis B Vaccination and Life Expectancy:
#There is a positive association between Hepatitis B vaccination coverage and life expectancy.
#For each additional percentage point increase in Hepatitis B vaccination coverage, life expectancy increases by approximately 0.069 years (or about 25 days).
#The relationship is statistically significant (p-value < 2e-16), though the R-squared value is quite low (about 4%), indicating that Hepatitis B vaccination rates alone do not explain most of the variation in life expectancy.

#now to visually show the relation between the Hepatitis B Vaccination and Life Expectancy we use the scatter plot:

# Scatter plot with regression line for Hepatitis B and Life Expectancy
ggplot(Life_expectancy, aes(x = `Hepatitis.B`, y = `Life.expectancy`)) +
  geom_point() +
  geom_smooth(method = "lm", color = "blue") +
  labs(title = "Hepatitis B Vaccination vs. Life Expectancy",
       x = "Hepatitis B Vaccination Coverage (%)",
       y = "Life Expectancy (Years)") +
  theme_minimal()
#Insights from the scatter plot we see that:

#Positive Correlation: There is a clear positive correlation between Hepatitis B vaccination coverage and life expectancy, emphasizing the importance of vaccinations in public health initiatives.

#High Coverage Clustering: A large cluster of countries with vaccination rates near 100% suggests successful vaccination programs in those regions, possibly due to effective healthcare policies.

#Variable Life Expectancy: Significant variability in life expectancy at lower vaccination rates indicates that other health determinants are at play beyond Hepatitis B vaccination.

#Outlier Identification: Outliers, particularly at lower vaccination rates with unexpectedly high life expectancy, may point to unique health factors or reporting inaccuracies that warrant further investigation.

#Ceiling Effect: The leveling off of life expectancy at higher vaccination rates suggests diminishing returns and potential saturation, highlighting the need for diversified health strategies beyond a certain threshold of vaccination coverage.

#Predictive Limitations: The spread of data points around the trend line demonstrates the limitations of using vaccination rates alone to predict life expectancy, advocating for a more holistic approach in health analytics.

#Strategic Insights: The relationship depicted in the scatter plot provides strategic insights for healthcare stakeholders to invest in comprehensive vaccination programs as a pathway to enhance population longevity


# Linear model for Polio vaccination
model_polio <- lm(`Life.expectancy` ~ Polio, data = Life_expectancy)
summary(model_polio)

# Scatter plot with regression line for Polio and Life Expectancy
ggplot(Life_expectancy, aes(x = `Polio`, y = `Life.expectancy`)) +
  geom_point() +
  geom_smooth(method = "lm", color = "blue") +
  labs(title = "Polio vs. Life Expectancy",
       x = "Polio Vaccination Coverage (%)",
       y = "Life Expectancy (Years)") +
  theme_minimal()
#Insights from the scatter plot of Polio vs Life expectancy:

#General Trend: A positive trend line in the plot highlights a correlation where higher Polio vaccination coverage is associated with increased life expectancy across the dataset.

#Data Density: Most data points are densely populated towards the higher end of vaccination coverage, suggesting that a majority of the countries have high Polio vaccination rates.

#Varying Efficacy: There's considerable spread in life expectancy at the lower end of vaccination coverage, indicating varying efficacy of other health interventions and policies across countries.

#Potential Plateau: The flatter slope at the upper end of the vaccination coverage may indicate a plateau effect, where increases in vaccination coverage above a certain level do not correspond to significant increases in life expectancy.

#Investment Opportunities: For countries with lower vaccination rates and life expectancy, there are potential opportunities for impactful investments in healthcare infrastructure and vaccination programs.

#Strategic Health Planning: The insights from the trend could be used for strategic health planning, advocating for increased Polio vaccination as part of broader public health initiatives to improve life expectancy.

# Linear model for Diphtheria vaccination
model_diphtheria <- lm(`Life.expectancy` ~ Diphtheria, data = Life_expectancy)
summary(model_diphtheria)

# Scatter plot with regression line for Diphtheria and Life Expectancy
ggplot(Life_expectancy, aes(x = `Diphtheria`, y = `Life.expectancy`)) +
  geom_point() +
  geom_smooth(method = "lm", color = "blue") +
  labs(title = "Diphtheria vs. Life Expectancy",
       x = "Diphtheria Vaccination Coverage (%)",
       y = "Life Expectancy (Years)") +
  theme_minimal()

#Insights from the scatter plot of Polio vs Life expectancy:

#Positive Correlation: The data indicates a positive correlation between Diphtheria vaccination coverage and life expectancy, suggesting that higher immunization rates contribute to longer life spans.

#Health Investment Indication: Dense clustering at higher vaccination levels points to widespread health investments in Diphtheria immunization globally.

#Inequality Implication: The variation in life expectancy at lower vaccination rates may highlight health inequality, suggesting areas where health services could be improved.

#Diminishing Returns: The trend line shows possible diminishing returns for life expectancy gains in high vaccination rate regions, implying other factors influence life expectancy as countries approach full vaccination.

#Strategic Health Interventions: Opportunities for strategic health interventions are implied for regions with lower vaccination coverage to potentially enhance overall life expectancy.

#Data-Driven Decision Making: For policymakers and health strategists, the visual data underscores the importance of vaccination programs as a key element of public health planning.


#By the Vacination rates in the countries we have seen a positive indication in most of the countries.
#Now let's see how the facilities in countries (i.e GDP) is related to the life expectancy

model_gdp <- lm(`Life.expectancy` ~ GDP, data = Life_expectancy)
summary(model_gdp)

# Scatter plot with regression line for GDP and Life Expectancy
ggplot(Life_expectancy, aes(x = `GDP`, y = `Life.expectancy`)) +
  geom_point() +
  geom_smooth(method = "lm", color = "blue") +
  labs(title = "GDP vs. Life Expectancy",
       x = "GDP",
       y = "Life Expectancy (Years)") +
  theme_minimal()

#Insights from the scatter plot

#Positive Correlation: The data indicates a positive correlation; as GDP per capita increases, so does life expectancy, underscoring the relationship between economic status and health outcomes.

#Concentration of Data: Most data points are concentrated below a GDP of $25,000, reflecting the economic reality that many countries fall into the low to lower-middle-income bracket.

#Outliers Identified: There are outliers with high life expectancy at lower GDP levels, possibly due to effective public health policies or other non-economic factors that contribute to a healthier population.

#Diminishing Returns: The graph suggests diminishing returns at higher GDP levels, where increases in GDP have less impact on extending life expectancy, pointing to the complexity of factors that influence health.

#Policy Recommendation: To advise health organizations in lower-income countries, a focus on cost-effective public health interventions that can lead to significant health benefits, such as vaccination programs, clean water initiatives, and basic healthcare services, would be key.

#Business Strategy for Health Organizations: For profitability and sustainability, health organizations could adopt a volume-based approach, tailoring low-cost solutions to large populations. Additionally, they could seek partnerships with governments and international health agencies for funding and support.

#Let's look how the alcohol effects the Life expectancy in various countries:

model_alcohol <- lm(`Life.expectancy` ~ Alcohol, data = Life_expectancy)
summary(model_alcohol)

# Scatter plot with regression line for Alcohol and Life Expectancy
ggplot(Life_expectancy, aes(x = `Alcohol`, y = `Life.expectancy`)) +
  geom_point() +
  geom_smooth(method = "lm", color = "blue") +
  labs(title = "Alcohol vs. Life Expectancy",
       x = "Alcohol",
       y = "Life Expectancy (Years)") +
  theme_minimal()

#Insights from the scatter plot Alcohol Vs Life Expectancy:
#General Uptrend: There appears to be a general uptrend suggesting that, initially, higher alcohol consumption per capita is associated with higher life expectancy. 
#This might reflect social aspects of moderate drinking or could correlate with wealthier countries where alcohol consumption and healthcare systems are better.

#Variability at Low Consumption: Significant variability in life expectancy is observed at lower levels of alcohol consumption, indicating that factors other than alcohol significantly influence health outcomes in these populations.

#Potential Inversion Point: At higher levels of alcohol consumption, the trend may plateau or even decline, which could suggest that beyond a certain point, the negative health impacts of alcohol may outweigh any positive social or economic correlations.

#Investment Insight: The scatter plot hints at potential investment opportunities for health organizations in education and intervention programs aimed at responsible alcohol consumption.

#Preventive Measures: Health organizations could focus on preventive measures in countries with higher alcohol consumption rates, where the negative health impacts may begin to offset the positive correlation with life expectancy.

#Strategic Health Campaigns: There is room for strategic health campaigns that address the risks of excessive alcohol consumption and promote healthier lifestyles, particularly in regions where the consumption is notably high.


#Let's see how the health expenditure in countries prevent the infant deaths.
#The data points are mainly concentrated at the lower end. Hence making it difficult 
# Applying log transformation directly if there are no zero or negative values.
Life_expectancy$log_infant_deaths <- log(Life_expectancy$infant.deaths)
Life_expectancy$log_percentage_expenditure <- log(Life_expectancy$percentage.expenditure)

# Plotting with log-transformed axes
ggplot(Life_expectancy, aes(x = log_infant_deaths, y = log_percentage_expenditure)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", color = "blue") +
  scale_x_continuous(name = "Infant Deaths", 
                     breaks = scales::trans_breaks("log10", function(x) 10^x),
                     labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  scale_y_continuous(name = "Health Expenditure (%)", 
                     breaks = scales::trans_breaks("log10", function(x) 10^x),
                     labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  labs(title = "Log-Log Plot of Health Expenditure vs. Infant Deaths") +
  theme_minimal()

#Insights from the plot displaying the relation between Health Expenditure and Infant Deaths.

#Transformed Scale: The use of a logarithmic scale on both axes has normalized the distribution of data, which allows us to better visualize the relationship between health expenditure and infant mortality.

#Downward Trend: The plot shows a downward trend indicating that as health expenditure increases, infant deaths tend to decrease. This is in line with expectations that higher investment in health correlates with better health outcomes.

#Elasticity Indication: The log-log model provides an elasticity interpretation; a percentage change in health expenditure is associated with a percentage change in infant deaths.

#Data Clustering: There is a clustering of data points at the lower end of infant deaths, which suggests that many countries have relatively low infant mortality rates.

#Outliers Presence: There are some apparent outliers, particularly a few cases with high health expenditure but also high infant mortality. These cases may warrant further investigation to understand the factors contributing to this discrepancy.

#Policy Implications: For health organizations and policymakers, the overall trend supports the notion that increasing health expenditure is an effective strategy to reduce infant mortality.

# Multiple Regression Model
economic_model <- lm(Life.expectancy ~ GDP + `Income.composition.of.resources` + `percentage.expenditure`, data = Life_expectancy)
summary(economic_model)

#Insights from the summary of regression model

#Income Composition Significance: The significant impact of income composition on resources suggests that it's a key factor in national life expectancy levels, possibly reflecting the overall development and social factors that contribute to health.

#Secondary Role of GDP: While GDP per capita appears to contribute to life expectancy, it is not the most critical factor within this model, suggesting that simply increasing a nation's wealth may not be sufficient to improve health outcomes.

#Less Impact from Health Spending: The percentage of health expenditure's non-significant coefficient suggests that how much is spent may be less important than how it is spent. The efficiency and targeting of health spending might be areas for health organizations to explore.

#Strategic Investments: Health organizations should consider strategic investments that align with improving a country's income composition of resources, as this could have a broader and more significant impact on improving life expectancy.

#Holistic Approaches: The model and plot combined underline the importance of a holistic approach that encompasses economic development, social policies, and targeted health expenditure to improve life expectancy.

#Data-Driven Decisions: The significant statistical figures and the positive associations observed in the plot reinforce the need for data-driven decisions in policy formulation and health program development.




