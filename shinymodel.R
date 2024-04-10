# Load Dataset
read.csv("C:/Users/ASUS/OneDrive/Desktop/OUR WORLD COVID 19 DATASET.csv", header=FALSE)

# dw is the data frame in which we are storing our dataset
dw <- read.csv("C:/Users/ASUS/OneDrive/Desktop/OUR WORLD COVID 19 DATASET.csv")

# used dim() to know the dimension of dataset
dim(dw)  

# used sapply() to get the class of each column
sapply(dw, class)
# Displaying data types
print(sapply(dw, class))

#used names() to get the names of the columns in the data frame dw
names(dw)

# Converting string date to date-time
dw$date <- as.Date(dw$date)

# to get the first 5 rows of data frame
head(dw)

# Converting date-time to ordinal
dw$date <- as.integer(as.Date(dw$date, origin = "1970-01-01"))
head(dw)

# str() provides information about the structure of the data frame, including the names, classes, and first few observations of each column.
str(dw)

# to check the null values in data set, passing data frame dw to na()
any(is.na(dw))

# print the names of columns that contain null values 
null_columns <- colnames(dw)[colSums(is.na(dw)) > 0]

# Print the names of columns with null values
print(null_columns)

# sum of NA values in each column
missing_values_sum <- colSums(is.na(dw))

# Print the sum of missing values in each column
print(missing_values_sum) 

# Dropping few columns that are not important for the model
dw <- subset(dw, select = -c(gdp_per_capita,handwashing_facilities , human_development_index,
                             life_expectancy ,male_smokers,female_smokers ))
print(dw)
head(dw)

# Handling NA values 
# used lib zoo as it handles, manipulates, and analyses the time series data
library(zoo)
# na.aggregate() we have used to replace missing values in the column dw$total_cases with the mean value of the non-missing values in that column.
dw$total_cases <- na.aggregate(dw$total_cases, FUN = mean)
dw$new_cases <- na.aggregate(dw$new_cases, FUN = mean)
dw$new_cases_smoothed <- na.aggregate(dw$new_cases_smoothed, FUN = mean)
dw$total_deaths <- na.aggregate(dw$total_deaths, FUN = mean)
dw$new_deaths <- na.aggregate(dw$new_deaths, FUN = mean)
dw$new_deaths_smoothed <- na.aggregate(dw$new_deaths_smoothed, FUN = mean)
dw$total_cases_per_million <- na.aggregate(dw$total_cases_per_million, FUN = mean)
dw$new_cases_per_million <- na.aggregate(dw$new_cases_per_million, FUN = mean)
dw$new_cases_smoothed_per_million <- na.aggregate(dw$new_cases_smoothed_per_million, FUN = mean)
dw$total_deaths_per_million <- na.aggregate(dw$total_deaths_per_million, FUN = mean)
dw$new_deaths_per_million <- na.aggregate(dw$new_deaths_per_million, FUN = mean)
dw$new_deaths_smoothed_per_million <- na.aggregate(dw$new_deaths_smoothed_per_million, FUN = mean)
dw$icu_patients<- na.aggregate(dw$icu_patients, FUN = mean)
dw$icu_patients_per_million <- na.aggregate(dw$icu_patients_per_million, FUN = mean)
dw$hosp_patients <- na.aggregate(dw$hosp_patients, FUN = mean)
dw$hosp_patients_per_million <- na.aggregate(dw$hosp_patients_per_million, FUN = mean)
dw$weekly_icu_admissions <- na.aggregate(dw$weekly_icu_admissions, FUN = mean)
dw$weekly_icu_admissions_per_million <- na.aggregate(dw$weekly_icu_admissions_per_million, FUN = mean)
dw$weekly_hosp_admissions <- na.aggregate(dw$weekly_hosp_admissions, FUN = mean)
dw$weekly_hosp_admissions_per_million <- na.aggregate(dw$weekly_hosp_admissions_per_million, FUN = mean)
dw$total_tests <- na.aggregate(dw$total_tests, FUN = mean)
dw$new_tests <- na.aggregate(dw$new_tests, FUN = mean)
dw$total_tests_per_thousand <- na.aggregate(dw$total_tests_per_thousand, FUN = mean)
dw$new_tests_per_thousand <- na.aggregate(dw$new_tests_per_thousand, FUN = mean)
dw$new_tests_smoothed <- na.aggregate(dw$new_tests_smoothed, FUN = mean)
dw$new_tests_smoothed_per_thousand <- na.aggregate(dw$new_tests_smoothed_per_thousand, FUN = mean)
dw$positive_rate <- na.aggregate(dw$positive_rate, FUN = mean)
dw$tests_per_case <- na.aggregate(dw$tests_per_case, FUN = mean)
dw$total_vaccinations <- na.aggregate(dw$total_vaccinations, FUN = mean)
dw$people_vaccinated <- na.aggregate(dw$people_vaccinated, FUN = mean)
dw$people_fully_vaccinated <- na.aggregate(dw$people_fully_vaccinated, FUN = mean)
dw$total_boosters <- na.aggregate(dw$total_boosters, FUN = mean)
dw$new_vaccinations <- na.aggregate(dw$new_vaccinations, FUN = mean)
dw$new_vaccinations_smoothed <- na.aggregate(dw$new_vaccinations_smoothed, FUN = mean)
dw$total_vaccinations_per_hundred <- na.aggregate(dw$total_vaccinations_per_hundred, FUN = mean)
dw$people_vaccinated_per_hundred <- na.aggregate(dw$people_vaccinated_per_hundred, FUN = mean)
dw$people_fully_vaccinated_per_hundred <- na.aggregate(dw$people_fully_vaccinated_per_hundred, FUN = mean)
dw$total_boosters_per_hundred <- na.aggregate(dw$total_boosters_per_hundred, FUN = mean)
dw$new_vaccinations_smoothed_per_million <- na.aggregate(dw$new_vaccinations_smoothed_per_million, FUN = mean)
dw$new_people_vaccinated_smoothed <- na.aggregate(dw$new_people_vaccinated_smoothed, FUN = mean)
dw$new_people_vaccinated_smoothed_per_hundred <- na.aggregate(dw$new_people_vaccinated_smoothed_per_hundred, FUN = mean)
dw$stringency_index <- na.aggregate(dw$stringency_index, FUN = mean)
dw$population_density <- na.aggregate(dw$population_density, FUN = mean)
dw$median_age <- na.aggregate(dw$median_age, FUN = mean)
dw$aged_65_older <- na.aggregate(dw$aged_65_older, FUN = mean)
dw$aged_70_older <- na.aggregate(dw$aged_70_older, FUN = mean)
dw$extreme_poverty <- na.aggregate(dw$extreme_poverty, FUN = mean)
dw$reproduction_rate <- na.aggregate(dw$reproduction_rate, FUN = mean)
dw$cardiovasc_death_rate <- na.aggregate(dw$cardiovasc_death_rate, FUN = mean)
dw$diabetes_prevalence <- na.aggregate(dw$diabetes_prevalence, FUN = mean)
dw$hospital_beds_per_thousand <- na.aggregate(dw$hospital_beds_per_thousand, FUN = mean)
dw$excess_mortality_cumulative_absolute <- na.aggregate(dw$excess_mortality_cumulative_absolute, FUN = mean)
dw$excess_mortality_cumulative <- na.aggregate(dw$excess_mortality_cumulative, FUN = mean)
dw$excess_mortality_cumulative_per_million <- na.aggregate(dw$excess_mortality_cumulative_per_million, FUN = mean)
dw$excess_mortality <- na.aggregate(dw$excess_mortality, FUN = mean)
dw$excess_mortality_cumulative_per_million <- na.aggregate(dw$excess_mortality_cumulative_per_million, FUN = mean)

#after handling NA values , checking whether we have any other NA values or not
any(is.na(dw))
null_columns <- colnames(dw)[colSums(is.na(dw)) > 0]
# Print the names of columns with null values
print(null_columns)


#calulating co-relation coefficient of few features to understand the how stronlg relationship between these two variables
cor(dw$total_cases, dw$total_deaths) 
#after calculating the co-relation coefficient between total cases and total deaths , we found that they are very strong relation of approx 93%(0.9397676)

# for below all features , relationship is not so strong , mostly in negative or very less
cor(dw$excess_mortality, dw$total_deaths) 
cor(dw$total_cases, dw$excess_mortality) 
cor(dw$excess_mortality_cumulative_absolute, dw$total_deaths) 
cor(dw$total_tests,dw$total_deaths)
cor(dw$reproduction_rate, dw$total_deaths) 
cor(dw$hosp_patients, dw$total_deaths) 
cor(dw$new_deaths,dw$total_cases)

# Load required libraries
# used ggplot2 to create plots, dplyr for data manipulation tasks and stats to do the statistical analysis
library(ggplot2)
library(dplyr)
library(stats)


# 1. Correlation Analysis between total cases and total deaths , already seen above
correlation <- cor(dw$total_cases, dw$total_deaths)
print(paste("Correlation between total cases and total deaths:", correlation))
options(scipen=999)

# 2. Scatter Plot between total cases and total deaths 
ggplot(dw, aes(x = total_cases, y = total_deaths)) +
  geom_point() +
  labs(x = "Total Cases", y = "Total Deaths", title = "Scatter Plot of Total Cases vs Total Deaths")

# 3. Box Plot between median_age and total cases
ggplot(dw, aes(x = median_age, y = total_cases)) +
  geom_boxplot() +
  labs(x = "Median Age", y = "Total Cases", title = "Box Plot of Total Cases by Age Group")

# 4. Chi-Square Test to in analyze the categorical data to determine whether there is a relationship between two categorical variables.
# Created a contingency table to know the frequency distribution of categorical variables
contingency_table <- table(dw$location, dw$continent)

# Performed Chi-Square Test
chi_square_test <- chisq.test(contingency_table)
print(chi_square_test)
# here,the X-squared value is quite large(2240970),indicating a large difference between the observed and expected frequencies
# the p-value is < 0.00000000000000022, which is extremely small, indicating strong evidence against the null hypothesis.


# 5. Correlation Heatmap
correlation_matrix <- cor(dw[, c("total_cases", "total_deaths", "total_tests")])
library(reshape2)
# Plot heatmap
ggplot(data = melt(correlation_matrix), aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient(low = "blue", high = "red") +
  labs(title = "Correlation Heatmap", x = "Variable 1", y = "Variable 2")



# Load necessary libraries
library(shiny)
library(DT)
library(readr)
library(caret)
library(randomForest)
library(shinyjs)  # Add this line
library(ggplot2)
library(dplyr)

# Convert 'date' column to Date format
dw$date <- as.Date(dw$date)  

# UI
ui <- fluidPage(
  titlePanel("COVID-19 Death Prediction"),
  sidebarLayout(
    sidebarPanel(
      selectInput("country", "Select Country:", choices = unique(dw$location)),
      sliderInput("total_cases", "Select Total Cases:", min = 0, max = 1000000, value = 1000),
      actionButton("predict_button", "Predict"),
      br(),
      h5("Predicted Deaths:"),
      textOutput("prediction"),
      br(),
      h5("Model Summary:"),
      verbatimTextOutput("model_summary")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Total Cases vs Total Deaths", plotOutput("data_plot")),
        tabPanel("Time Series Plot", plotOutput("time_series_plot"))
      )
    )
  )
)

# Server
server <- function(input, output) {
  
  # Function to predict deaths
  predict_deaths <- eventReactive(input$predict_button, {
    total_cases <- input$total_cases
    
    # Fit a simple linear regression model
    model <- lm(total_deaths ~ total_cases, data = subset(dw, location == input$country))
    
    # Get model summary
    model_summary <- summary(model)
    
    # Predict deaths based on input total cases
    predicted_deaths <- predict(model, newdata = data.frame(total_cases = total_cases))
    
    # Return predicted deaths and model summary
    return(list(predicted_deaths = round(predicted_deaths, 2), model_summary = model_summary))
  })
  
  # Output predicted deaths
  output$prediction <- renderText({
    predict_deaths()$predicted_deaths
  })
  
  # Output model summary
  output$model_summary <- renderPrint({
    predict_deaths()$model_summary
  })
  
  # Plot of total cases vs total deaths
  output$data_plot <- renderPlot({
    ggplot(subset(dw, location == input$country), aes(x = total_cases, y = total_deaths)) +
      geom_point() +
      geom_smooth(method = "lm") +
      labs(title = "Total Cases vs Total Deaths", x = "Total Cases", y = "Total Deaths") +
      theme_minimal()
  })
  
  
  # Time series plot
  output$time_series_plot <- renderPlot({
    filtered_data <- dw %>%
      filter(location == input$country) %>%
      group_by(date) %>%
      summarise(total_cases = sum(total_cases)) %>%
      na.omit()  # Remove NA values if any
    
    ggplot(filtered_data, aes(x = date, y = total_cases)) +
      geom_line() +
      labs(title = "Time Series Plot of Total Cases", x = "Date", y = "Total Cases") +
      theme_minimal()
  })
}

# Run the application
shinyApp(ui, server)
