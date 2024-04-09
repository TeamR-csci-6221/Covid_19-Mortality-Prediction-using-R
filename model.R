# load dataset

read.csv("C:/Users/ASUS/OneDrive/Desktop/OUR WORLD COVID 19 DATASET.csv", header=FALSE)
dw <- read.csv("C:/Users/ASUS/OneDrive/Desktop/OUR WORLD COVID 19 DATASET.csv")
dim(dw)                                                                  
sapply(dw, class)
names(dw)

# Converting string date to date-time
dw$date <- as.Date(dw$date)
head(dw)

print(sapply(dw, class))  # Displaying data types

# Converting date-time to ordinal
dw$date <- as.integer(as.Date(dw$date, origin = "1970-01-01"))
head(dw)
str(dw)

# Using sapply() function to get the class of each variable
sapply(dw, class)


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
library(zoo)
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

any(is.na(dw))
null_columns <- colnames(dw)[colSums(is.na(dw)) > 0]
# Print the names of columns with null values
print(null_columns)


#co-relation coefficient
cor(dw$total_cases, dw$total_deaths) 
cor(dw$excess_mortality, dw$total_deaths) 
cor(dw$total_cases, dw$excess_mortality) 
cor(dw$excess_mortality_cumulative_absolute, dw$total_deaths) 
cor(dw$total_tests,dw$total_deaths)
cor(dw$reproduction_rate, dw$total_deaths) 
cor(dw$hosp_patients, dw$total_deaths) 
cor(dw$new_deaths,dw$total_cases)

# Load required libraries
library(ggplot2)
library(dplyr)
library(stats)


# 1. Correlation Analysis
correlation <- cor(dw$total_cases, dw$total_deaths)
print(paste("Correlation between total cases and total deaths:", correlation))
options(scipen=999)

# 2. Scatter Plot
ggplot(dw, aes(x = total_cases, y = total_deaths)) +
  geom_point() +
  labs(x = "Total Cases", y = "Total Deaths", title = "Scatter Plot of Total Cases vs Total Deaths")

# 3. Box Plot
ggplot(dw, aes(x = median_age, y = total_cases)) +
  geom_boxplot() +
  labs(x = "Median Age", y = "Total Cases", title = "Box Plot of Total Cases by Age Group")

# 4. Chi-Square Test (Example with hypothetical data)
# Create a contingency table
contingency_table <- table(dw$location, dw$continent)

# Perform Chi-Square Test
chi_square_test <- chisq.test(contingency_table)
print(chi_square_test)

# 5. Correlation Heatmap
correlation_matrix <- cor(dw[, c("total_cases", "total_deaths", "total_tests")])
library(reshape2)

# Plot heatmap
ggplot(data = melt(correlation_matrix), aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient(low = "blue", high = "red") +
  labs(title = "Correlation Heatmap", x = "Variable 1", y = "Variable 2")


india_cases <- subset(dw, location == "India")
head(india_cases)
tail(india_cases)

# Compute quantiles
quantiles <- quantile(dw$new_cases, probs = c(0.5, 0.95, 0.99))

# Subsetting
in_new_case <- dw[india_cases$new_cases < quantiles[3], ]

# Create the scatterplot with a regression line
ggplot(data = in_new_case, aes(x = new_cases, y = total_cases)) +
 geom_point(color = "orange") +  # Points
 geom_smooth(method = "lm", se = FALSE, color = "orange") +  # Regression line without confidence interval
 labs(x = "New Cases", y = "Total Cases") +  # Labels for axes
  theme_minimal()  # Minimal theme


# Load necessary libraries
library(shiny)
library(DT)
library(readr)
library(caret)
library(randomForest)
library(shinyjs)  # Add this line


# UI
ui <- fluidPage(
  titlePanel("COVID-19 Deaths Prediction"),
  sidebarLayout(
    sidebarPanel(
      textInput("country", "Enter Country:", ""),
      numericInput("confirmed_cases", "Enter Confirmed Cases:", 0),
      actionButton("predict_button", "Predict"),
      hr(),
      textOutput("prediction_output")
    ),
    mainPanel(
      DTOutput("table")
    )
  )
)

# Server
server <- function(input, output, session) {  # Add session here
  
  # Display datatable
  output$table <- renderDT({
    datatable(dw, options = list(pageLength = 10))
  })
  
  # Predict function
  observeEvent(input$predict_button, {
    # Disable the button to prevent multiple clicks
    disable("predict_button")  # Use shinyjs function
    
    country <- input$country
    confirmed_cases <- input$total_cases  # corrected input variable name
    
    # Filter data for selected country
    selected_country <- dw[dw$location == country, ]
    
    # Check if country exists in dataset
    if (nrow(selected_country) > 0) {
      # Prepare data for modeling
      X <- selected_country$total_cases
      y <- selected_country$total_deaths
      
      # Train linear regression model
      model <- lm(y ~ X)
      
      # Predict deaths
      predicted_deaths <- predict(model, newdata = data.frame(X = confirmed_cases))
      
      # Output prediction
      output$prediction_output <- renderText({
        paste("Predicted Deaths:", round(predicted_deaths, 2))
      })
    } else {
      # Output message if country not found
      output$prediction_output <- renderText({
        "Country not found in the dataset."
      })
    }
    
    # Enable the button after the prediction process is completed
    enable("predict_button")  # Use shinyjs function
  })
}

# Run the application
shinyApp(ui, server)