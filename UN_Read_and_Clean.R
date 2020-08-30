# Import packages
library(tidyr)
library(dplyr)
library(ggplot2)

# Read United Nations Millennium Development Goals csv file
filename <- 'TrainingSet.csv'
UN_data <- read.csv(filename)

# Display a portion of the data to check for problems
summary(UN_data)
str(UN_data)

#Drop X and Series.Code columns.  They don't provide any useful information
UN_data <- UN_data %>% select(-c(X,Series.Code))

# The years should not be variables, but rather values of a variable
# Pivot the data so the years are now values rather than columns
UN_data <- gather(UN_data, key="year",value="value", `X1972..YR1972.`:`X2007..YR2007.`)

# Clean the text of the year column
UN_data$year <- substring(UN_data$year,2,5)
UN_data$year <- as.integer(UN_data$year)

# Rename Country.Name to country_name and Series.Name to series_name.  Including periods in column names is bad practice
UN_data <- UN_data %>%
    rename(country_name = Country.Name,
           series_name = Series.Name)

# Based on the principles of tidy data, each variable must be its own column
# Currently, the values of series_name actually represent variables 
# Spread the datafame so each Series.Name is a column
UN_data <- spread(UN_data,key="series_name",value="value")

# Review the datatypes of each column
str(UN_data)

# It will be more helpful in our EDA if country_name values are factors
UN_data$country_name = as.factor(UN_data$country_name)

# Although there are many NA values in the dataset, removing observations containing
# NA values would remove each observation.  For now, it is best to leave the NA values as-is


G7 <- c("United States","Canada","France","Germany","Italy","United Kingdom")

G7_DataFrame <- UN_data[UN_data$country_name %in% G7,]


# Find number of NA values per column
column_na_values <- data.frame()
for(column in c(1:ncol(G7_DataFrame))){
  column_na_values[column,1]<-names(G7_DataFrame)[column]
  column_na_values[column,2] <- sum(is.na(G7_DataFrame[,column]))
}
colnames(column_na_values) <- c('Series_Name','Number_NA_Values')

# Order the results to find rows with the least NA values
columns_no_na <- column_na_values[column_na_values$Number_NA_Values==0,]

# Subset columns of choice for EDA
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

G7_DataFrame <- G7_DataFrame[,columns]

# Write cleaned dataframe to a new CSV file
new_path <- 'cleaned_UN_data.csv'
write.csv(G7_DataFrame,new_path, row.names = FALSE)
