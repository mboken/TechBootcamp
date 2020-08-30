# UN EDA
# 8/23/2020

#import libraries
library(dplyr)
library(ggplot2)

# Read in cleaned dataset
G7_dataframe <- read.csv('cleaned_UN_data.csv')

# Reset the column names
columns <- c("country_name",
             "year",
             "Adjusted net national income per capita (current US$)",
             "Adolescent fertility rate (births per 1,000 women ages 15-19)",
             "Life expectancy at birth, total (years)",
             "CO2 emissions from transport (million metric tons)",
             "Agricultural land (sq. km)",
             "Household final consumption expenditure (current US$)",
             "Population density (people per sq. km of land area)",
             "Primary education, duration (years)",
             "Total reserves (includes gold, current US$)",
             "Total reserves minus gold (current US$)",
             "Secondary education, duration (years)"
)

names(G7_dataframe) <- columns

# reset country_name as factor
G7_dataframe$country_name <- as.factor(G7_dataframe$country_name)

# Find correlation between selected variables
#Code for find correlation
df<-G7_dataframe %>% select('Adjusted net national income per capita (current US$)',
                            "Adolescent fertility rate (births per 1,000 women ages 15-19)",
                            'Adolescent fertility rate (births per 1,000 women ages 15-19)',
                            'Life expectancy at birth, total (years)',
                            'CO2 emissions from transport (million metric tons)',
                            "Agricultural land (sq. km)",
                            'Household final consumption expenditure (current US$)',
                            'Population density (people per sq. km of land area)',
                            'Primary education, duration (years)','Total reserves (includes gold, current US$)',
                            'Total reserves minus gold (current US$)',
                            'Secondary education, duration (years)')
cor(df,use='complete.obs')

#Scatterplot for variables
pairs(df$`Adjusted net national income per capita (current US$)`~df$`CO2 emissions from transport (million metric tons)`+df$`Agricultural land (sq. km)`+df$`Household final consumption expenditure (current US$)`+df$`Total reserves (includes gold, current US$)`)

#regression model for variables with high correlation to net income
model <- lm(df$`Adjusted net national income per capita (current US$)`~df$`CO2 emissions from transport (million metric tons)`+df$`Agricultural land (sq. km)`+df$`Household final consumption expenditure (current US$)`+df$`Total reserves (includes gold, current US$)`)

summary(model)



# Plotting CO2 emissions for countries over time
ggplot(data=G7_dataframe, aes(x = year, y = `CO2 emissions from transport (million metric tons)`, color = country_name))+
  geom_point() + 
  ggtitle('Trend in CO2 Emissions over Time in G7 Countries')

# Trend in Adjusted Net Income per Capita over Time for G7 Countries
ggplot(data=G7_dataframe, aes(x = year, y = `Adjusted net national income per capita (current US$)`, color = country_name))+
  geom_point() + 
  ggtitle('Trend in Adjusted Net Income per Capita over Time for G7 Countries')

# Relationship between Adjusted National Income per Capita and Life Expectancy at Birth
ggplot(data=G7_dataframe, aes(x = `Life expectancy at birth, total (years)`, y = `Adjusted net national income per capita (current US$)`, color = country_name))+
  geom_point() + 
  ggtitle('Relationship between Adjusted National Income per Capita and Life Expectancy at Birth')

# Adjusted Net National Income per Capita over Household Final Consumption Expenditure
ggplot(data=G7_dataframe, aes(x = `Household final consumption expenditure (current US$)`, y = `Adjusted net national income per capita (current US$)`, color = country_name))+
  geom_point() +
  ggtitle('Adjusted Net National Income per Capita over Household Final Consumption Expenditure')

# find average  net income for each country in the dataframe (grouped by years)
G7_groupby_country <-  G7_dataframe %>% group_by(country_name) %>% 
  summarise_at(vars(`Adjusted net national income per capita (current US$)`),list(`Average Adjusted net national income per capita (current US$)`=mean)) 
 
# plot a bar graph of the average net income for each country between the years 1972-2007
ggplot(data=G7_groupby_country, aes(x = country_name, y = `Average Adjusted net national income per capita (current US$)`)) + 
  geom_bar(stat='identity') + 
  ggtitle('Average Adjusted net National Income Per Capita (1972 - 2007)')


# SUMMARY STATISTICS
# Overall descriptive statistics:
str(G7_dataframe)

# Adjusted net national income per capita
summary(G7_dataframe$`Adjusted net national income per capita (current US$)`)

# Life expectancy at birth
summary(G7_dataframe$`Life expectancy at birth, total (years)`)

# Household final consumption expenditure
summary(G7_dataframe$'Household final consumption expenditure (current US$)')

# Total reserves (includes gold, current US$) 
summary(G7_dataframe$`Total reserves (includes gold, current US$)`)
