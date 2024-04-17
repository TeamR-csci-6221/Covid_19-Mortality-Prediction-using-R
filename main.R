library(wbstats)
# library(gifski)

# dw is the data frame in which we are storing our dataset
dw <- read.csv("/Users/dog/Documents/csci_6221/owid-covid-data.csv")

plotting_data <- dw

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


library(shiny)
library(ggplot2)
library(dplyr)
library(leaflet)
library(jsonlite)
library(rnaturalearth)
library(rnaturalearthdata)
library(plotly)
library(tidyr)


# Convert the date column to Date format
plotting_data$date <- as.Date(plotting_data$date)


unique_locations <- unique(plotting_data$location)
metrics <- c("New Cases", "New Deaths", "Total Cases", "Total Deaths", "Vaccinated Per Hundred", "Fully Vaccinated Per Hundred", "Weekly Hospital_Admission Per Million", "Weekly ICU_Admission Per Million", "Positive Rate", "Total Test", "New Test", "Total Test Per Thousand","New Test Per Thousand")

ui <- fluidPage(
  
  # Styling the title panel
  tags$head(
    tags$style(
      HTML("
      .title-panel {
        background-color: #3498db;
        color: #fff;
        padding: 10px;
        border-radius: 5px;
        margin-bottom: 20px;
        text-align: center;
      }
      .center-panel {
        display: flex;
        justify-content: center;
        align-items: center;
      }
      body {
        background-color: #f0f3f5;
      }
      .btn-primary {
        background-color: #3498db;
        border-color: #3498db;
      }
      .btn-primary:hover {
        background-color: #2980b9;
        border-color: #2980b9;
      }
      .btn-secondary {
        background-color: #6c757d;
        border-color: #6c757d;
      }
      .btn-secondary:hover {
        background-color: #5a6268;
        border-color: #5a6268;
      }
      .nav-tabs .nav-link {
        color: #3498db;
      }
      .nav-tabs .nav-link.active {
        background-color: #3498db;
        color: #fff;
      }
      .model-summary {
        color: #2980b9;
      }
      ")
    )
  ),
  
  div(class = "title-panel",
      titlePanel("Covid-19 Analysis App", windowTitle = "R-Power - Empowering Data Analysis")
  ),
  
  navbarPage("Team R",
             tabPanel("Country Comparator Tool",
                      sidebarLayout(
                        sidebarPanel(
                          uiOutput("location_ui"),
                          actionButton("add_location", "Add Another Country", icon = icon("plus"), class = "btn-secondary"),
                          selectInput("metric", "Select Metric", choices = metrics, selected = "New Cases"),
                          dateInput("start_date", "Start Date:", value = "2021-01-01"),
                          dateInput("end_date", "End Date:", value = "2021-12-31"),
                          actionButton("submit", "Submit", class = "btn-primary")
                        ),
                        mainPanel(
                          plotOutput("cases_plot"),
                          leafletOutput("world_map")
                        )
                      )
             ),
             tabPanel("Pearsons Correlation Heatmap",
                      mainPanel(
                        dateInput("date", "Select Date:", value = "2023-01-01"),
                        div(plotOutput("heatmap_plot"), class = "center-panel") # Center the heatmap plot
                      )
             ),
             tabPanel("Effect of Vaccination over Time",
                      sidebarLayout(
                        sidebarPanel(
                          uiOutput("location_ui_2"),
                          actionButton("add_location_2", "Add Another Country", icon = icon("plus"), class = "btn-secondary"),
                          actionButton("submit_2", "Submit", class = "btn-primary")
                        ),
                        mainPanel(
                          plotlyOutput("animatedPlot", height = "700px"),
                        )
                      )
             ),
             tabPanel("COVID-19 Prediction Model",
                      sidebarLayout(
                        sidebarPanel(
                          selectInput("country", "Select Country:", choices = unique(plotting_data$location)),
                          sliderInput("total_cases", "Select Total Cases:", min = 0, max = 1000000, value = 1000),
                          actionButton("predict_button", "Predict", class = "btn-primary"),
                          br(),
                          h5("Predicted Deaths:", style = "color: #2c3e50;"),
                          htmlOutput("prediction"),
                          br(),
                          div(
                            verbatimTextOutput("model_summary"),
                            class = "model-summary"
                          )
                        ),
                        mainPanel(
                          tabsetPanel(
                            tabPanel("Total Cases vs Total Deaths", plotOutput("data_plot")),
                            tabPanel("Time Series Plot", plotOutput("time_series_plot"))
                          )
                        )
                      )
             )
  )
)


server <- function(input, output) {
  
  locations <- reactiveValues(list = list("United States")) # Start with 1 default location
  locations2 <- reactiveValues(list = list("United States")) # Start with 1 default location
  
  # Create a mapping from country names to ISO codes
  country_to_iso_df <- unique(plotting_data[, c("location", "iso_code")])
  
  # Ensure the 'location' column is used to name elements in the 'ISO' column
  country_to_iso <- setNames(country_to_iso_df$iso_code, country_to_iso_df$location)
  
  observeEvent(input$add_location, {
    # Temporarily store the current selections to update the list accurately
    current_selections <- sapply(1:length(locations$list), function(i) {
      input[[paste0("location", i)]]
    }, USE.NAMES = FALSE)
    
    # Check if the UI has been rendered and selections made before adding
    if (length(current_selections) > 0) {
      locations$list <- current_selections
    }
    
    # Add a new default location ("United States") to the list
    locations$list <- c(locations$list, "United States")
  })
  
  output$location_ui <- renderUI({
    input_list <- lapply(1:length(locations$list), function(i) {
      # Check if it's the last location and there are more than one locations
      if(i == length(locations$list) && length(locations$list) > 1) {
        # For the last location (if not the only location), include the "Remove" button
        fluidRow(
          column(10,
                 selectInput(paste0("location", i), label = sprintf("Select Country %d", i),
                             choices = unique_locations, selected = locations$list[[i]])),
          column(2,
                 actionButton(paste0("remove_location", i), label = "", icon = icon("minus")))
        )
      } else {
        # For all other conditions, do not include the "Remove" button
        fluidRow(
          column(12,
                 selectInput(paste0("location", i), label = sprintf("Select Country %d", i),
                             choices = unique_locations, selected = locations$list[[i]]))
        )
      }
    })
    do.call(tagList, input_list)
  })
  
  observeEvent(input$add_location_2, {
    # Temporarily store the current selections to update the list accurately
    current_selections <- sapply(1:length(locations2$list), function(i) {
      input[[paste0("location2", i)]]
    }, USE.NAMES = FALSE)
    
    # Check if the UI has been rendered and selections made before adding
    if (length(current_selections) > 0) {
      locations2$list <- current_selections
    }
    
    # Add a new default location ("United States") to the list
    locations2$list <- c(locations2$list, "United States")
  })
  
  output$location_ui_2 <- renderUI({
    input_list <- lapply(1:length(locations2$list), function(i) {
      # Check if it's the last location and there are more than one locations
      if(i == length(locations2$list) && length(locations2$list) > 1) {
        # For the last location (if not the only location), include the "Remove" button
        fluidRow(
          column(10,
                 selectInput(paste0("location2", i), label = sprintf("Select Country %d", i),
                             choices = unique_locations, selected = locations2$list[[i]])),
          column(2,
                 actionButton(paste0("remove_location", i), label = "", icon = icon("minus")))
        )
      } else {
        # For all other conditions, do not include the "Remove" button
        fluidRow(
          column(12,
                 selectInput(paste0("location2", i), label = sprintf("Select Country %d", i),
                             choices = unique_locations, selected = locations2$list[[i]]))
        )
      }
    })
    do.call(tagList, input_list)
  })
  
  observe({
    lapply(1:length(locations$list), function(i) {
      observeEvent(input[[paste0("remove_location", i)]], {
        # Only remove the specific location associated with the remove button
        locations$list <- locations$list[-i]
      })
    })
  })
  
  observe({
    lapply(1:length(locations2$list), function(i) {
      observeEvent(input[[paste0("remove_location", i)]], {
        # Only remove the specific location associated with the remove button
        locations2$list <- locations2$list[-i]
      })
    })
  })
  
  observeEvent(input$submit_2, {
    selected_locations2 <- sapply(1:length(locations2$list), function(i) {
      input[[paste0("location2", i)]]
    })
    
    filtered_data <- plotting_data %>%
      select(location, date, total_vaccinations_per_hundred, new_deaths_smoothed_per_million, iso_code, positive_rate, population) %>%
      mutate(date = as.Date(date, format = "%Y-%m-%d")) %>%  # Ensure 'date' is a Date object, adjust format as necessary
      filter(location %in% selected_locations2,  # Filter for specific locations
             date >= as.Date("2021-02-01") & date <= as.Date("2022-07-01"))
    
    # clean_data <- filtered_data %>%
    #   mutate(
    #     total_vaccinations_per_hundred = ifelse(is.na(total_vaccinations_per_hundred), median(total_vaccinations_per_hundred, na.rm = TRUE), total_vaccinations_per_hundred),
    #     new_deaths_smoothed_per_million = ifelse(is.na(new_deaths_smoothed_per_million), median(new_deaths_smoothed_per_million, na.rm = TRUE), new_deaths_smoothed_per_million),
    #     positive_rate = ifelse(is.na(positive_rate), median(positive_rate, na.rm = TRUE), positive_rate)
    #   )
    
    clean_data <- filtered_data %>%
      group_by(location) %>%
      fill(total_vaccinations_per_hundred, new_deaths_smoothed_per_million, positive_rate, .direction = "down") %>%
      ungroup()
    
    output$animatedPlot <- renderPlotly({
      # Assuming 'data' is defined elsewhere and properly formatted
      data <- data.frame(clean_data)
      
      # First plot (top)
      p1 <- plot_ly(data,
                    x = ~total_vaccinations_per_hundred,
                    y = ~new_deaths_smoothed_per_million,
                    text = ~location,
                    ids = ~location,
                    frame = ~as.character(date),
                    type = 'scatter',
                    mode = 'lines+markers',  # Changed from 'markers' to 'lines+markers' to add paths
                    line = list(color = ~location, colorscale = 'Set2'),  # Line properties
                    color = ~location,
                    colors = "Set2",
                    legendgroup='group1',
                    showlegend = FALSE,
                    marker = list(
                      sizemode = 'diameter',
                      # size = 30,
                      size = 15 + data$population * 0.0000001,
                      line = list(width = 2, color = 'darkgrey')
                    )) %>%
        layout(
          xaxis = list(title = "", showticklabels = TRUE),  # Hide x-axis title but show tick labels
          yaxis = list(title = "New Deaths per Million", range = c(-5, 20.0)),
          margin = list(b = 10, t = 100))  # Adjust top margin to show title
      
      # Second plot (bottom)
      p2 <- plot_ly(data,
                    x = ~total_vaccinations_per_hundred,
                    y = ~positive_rate,
                    text = ~location,
                    ids = ~location,
                    frame = ~as.character(date),
                    type = 'scatter',
                    mode = 'lines+markers',  # Changed from 'markers' to 'lines+markers' to add paths
                    line = list(color = ~location, colorscale = 'Set2'),  # Line properties
                    color = ~location,
                    colors = "Set2",
                    legendgroup='group1',
                    showlegend = TRUE,
                    marker = list(
                      sizemode = 'diameter',
                      # size = 30,
                      size = 15 + data$population * 0.0000001,
                      line = list(width = 2, color = 'darkgrey')
                    )) %>%
        layout(
          xaxis = list(title = "Vaccination per Hundred"),  # Only the last plot keeps the x-axis title
          yaxis = list(title = "Positivity Rate (%)", range = c(-0.05, 0.60)),
          margin = list(t = 30))  # Adjust top margin
      
      # Combine both plots into a single subplot layout
      subplot(p1, p2, nrows = 2, shareX = TRUE, titleX = TRUE, titleY = TRUE, margin = 0.05) %>%
        layout(showlegend = TRUE) %>%
        animation_opts(frame = 0.01, redraw = TRUE) %>%
        animation_slider(currentvalue = list(prefix = "Date: "), hide = FALSE)
    })
    
  })
  
  observeEvent(input$submit, {
    req(input$metric, input$start_date, input$end_date)
    
    selected_locations <- sapply(1:length(locations$list), function(i) {
      input[[paste0("location", i)]]
    })
    
    # Convert selected country names to ISO codes using the mapping
    selected_iso_codes <- sapply(selected_locations, function(location) {
      # Assuming country_to_iso is a named vector where names are countries and values are ISO codes
      country_to_iso[location]
    }, USE.NAMES = FALSE)
    
    metric <- input$metric
    start_date <- input$start_date
    end_date <- input$end_date
    
    # Initialize an empty data frame for filtered data
    filtered_data <- data.frame()
    
    if (metric == "New Cases") {
      y_metric <- "new_cases"
    } else if (metric == "New Deaths") {
      y_metric <- "new_deaths"
    } else if (metric == "Total Cases") {
      y_metric <- "total_cases"
    }else if (metric == "Total Deaths") {
      y_metric <- "total_deaths"
    }else if (metric == "Vaccinated Per Hundred") {
      y_metric <- "people_vaccinated_per_hundred"
    }else if (metric == "Fully Vaccinated Per Hundred") {
      y_metric <- "people_fully_vaccinated_per_hundred"
    }else if (metric == "Weekly Hospital_Admission Per Million") {
      y_metric <- "weekly_hosp_admissions_per_million"
    }else if (metric == "Weekly ICU_Admission Per Million") {
      y_metric <- "weekly_icu_admissions_per_million"
    }else if (metric == "Positive Rate") {
      y_metric <- "positive_rate"
    }else if (metric == "Total Test") {
      y_metric <- "total_tests"
    }else if (metric == "New Test") {
      y_metric <- "new_tests"
    }else if (metric == "Total Test Per Thousand") {
      y_metric <- "total_tests_per_thousand"
    }else if (metric == "New Test Per Thousand") {
      y_metric <- "new_tests_per_thousand"
    }
    
    
    # Loop through selected locations to filter data and combine
    for (location in selected_locations) {
      location_data <- plotting_data[plotting_data$location == location & plotting_data$date >= start_date & plotting_data$date <= end_date, ]
      if (metric == "New Cases") {
        location_data <- location_data %>%
          filter(!is.na(new_cases) & new_cases != 0)
      } else if (metric == "New Deaths") {
        location_data <- location_data %>%
          filter(!is.na(new_deaths) & new_deaths != 0)
      } else if (metric == "Vaccinated Per Hundred") {
        location_data <- location_data %>%
          filter(!is.na(people_vaccinated_per_hundred) & people_vaccinated_per_hundred != 0)
      } else if (metric == "Fully Vaccinated Per Hundred") {
        location_data <- location_data %>%
          filter(!is.na(people_fully_vaccinated_per_hundred) & people_fully_vaccinated_per_hundred != 0)
      } else if (metric == "Weekly Hospital_Admission Per Million") {
        location_data <- location_data %>%
          filter(!is.na(weekly_hosp_admissions_per_million) & weekly_hosp_admissions_per_million != 0)
      } else if (metric == "Weekly ICU_Admission Per Million") {
        location_data <- location_data %>%
          filter(!is.na(weekly_icu_admissions_per_million) & weekly_icu_admissions_per_million != 0)
      } else if (metric == "Positive Rate") {
        location_data <- location_data %>%
          filter(!is.na(positive_rate) & positive_rate != 0)
      } else if (metric == "Total Test") {
        location_data <- location_data %>%
          filter(!is.na(total_tests) & total_tests != 0)
      } else if (metric == "New Test") {
        location_data <- location_data %>%
          filter(!is.na(new_tests) & new_tests != 0)
      } else if (metric == "Total Test Per Thousand") {
        location_data <- location_data %>%
          filter(!is.na(total_tests_per_thousand) & total_tests_per_thousand != 0)
      } else if (metric == "New Test Per Thousand") {
        location_data <- location_data %>%
          filter(!is.na(new_tests_per_thousand) & new_tests_per_thousand != 0)
      }
      filtered_data <- rbind(filtered_data, location_data)
    }
    
    # Plot the data
    output$cases_plot <- renderPlot({
      ggplot(filtered_data, aes(x = date, y = get(y_metric), color = location)) +
        geom_line() +
        labs(x = "Date", y = sprintf("Number of %s", metric),
             title = paste(metric, "in Selected Countries from", start_date, "to", end_date),
             color = "Country") +
        theme_minimal() +
        theme(axis.text.x = element_text(size = 12, face = "bold", angle = 45, hjust = 1),  # Bold and larger x-axis text with angle
              axis.text.y = element_text(size = 12, face = "bold"),  # Bold and larger y-axis text
              axis.title.x = element_text(size = 14, face = "bold"),  # Bold and larger x-axis title
              axis.title.y = element_text(size = 14, face = "bold"),  # Bold and larger y-axis title
              plot.title = element_text(size = 16, face = "bold", hjust = 0.5),  # Bold and larger plot title, centered
              legend.title = element_text(size = 12, face = "bold"),  # Bold and larger legend title
              legend.text = element_text(size = 12, face = "bold"))  # Bold and larger legend text
    })
    
    output$world_map <- renderLeaflet({
      countries_sf <- ne_countries(scale = "medium", returnclass = "sf")
      
      map <- leaflet() %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        setView(lat = 0, lng = 0, zoom = 2) # Default view
      
      # Ensure at least one country is selected before attempting to filter and highlight
      if (length(selected_iso_codes) > 0) {
        filtered_countries_sf <- countries_sf %>%
          filter(iso_a3 %in% selected_iso_codes)
        
        map <- map %>%
          addPolygons(data = filtered_countries_sf,
                      fillColor = "#ff7800", weight = 1,
                      color = "#555555", fillOpacity = 0.7,
                      highlightOptions = highlightOptions(weight = 2,
                                                          color = "#666666",
                                                          fillOpacity = 0.9,
                                                          bringToFront = TRUE))
      }
      
      map
    })
    
  })
  
  output$heatmap_plot <- renderPlot({
    # Filter data for the specified date
    date_filtered_data <- plotting_data %>% filter(date == as.Date(input$date))
    
    # Check if data is available for the selected date
    if (nrow(date_filtered_data) == 0) {
      return(NULL)  # Return NULL if no data is available
    }
    
    # Select the columns of interest for rows and columns in the heatmap
    row_variables <- date_filtered_data %>%
      select(total_deaths_per_million, total_cases_per_million,
             weekly_icu_admissions_per_million, weekly_hosp_admissions_per_million, cardiovasc_death_rate, excess_mortality_cumulative_per_million)
    
    col_variables <- date_filtered_data %>%
      select(people_fully_vaccinated_per_hundred, people_vaccinated_per_hundred,
             total_boosters_per_hundred, population_density,
             median_age, aged_65_older, aged_70_older, gdp_per_capita, extreme_poverty, diabetes_prevalence, female_smokers, male_smokers, life_expectancy,
             human_development_index)
    
    # Compute Pearson's correlation
    correlation_matrix <- cor(row_variables, col_variables, use = "pairwise.complete.obs", method = "pearson")
    
    # Convert the correlation matrix to a format suitable for ggplot2
    correlation_data <- as.data.frame(as.table(correlation_matrix))
    
    names(correlation_data) <- c("RowVariable", "ColVariable", "Correlation")
    
    # Generate the heatmap with adjusted theme settings for better visibility
    heatmap_plot <- ggplot(correlation_data, aes(x = RowVariable, y = ColVariable, fill = Correlation)) +
      geom_tile() +
      scale_fill_gradient2(low = "skyblue", high = "darkred", mid = "white", midpoint = 0) +
      # coord_fixed() +  # Keep the aspect ratio 1:1
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12, face = "bold"),
            axis.text.y = element_text(size = 12, face = "bold"),
            axis.title.x = element_text(size = 14, face = "bold"),
            axis.title.y = element_text(size = 14, face = "bold"),
            plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
            legend.title = element_text(size = 12, face = "bold"),
            legend.text = element_text(size = 12, face = "bold")) +
      labs(title = paste("Pearson's Correlation Heatmap on", input$date), x = "", y = "")
    
    print(heatmap_plot)
  }, width = 1200, height = 800, res = 96)  # Adjust width and height as needed
  
  
  predict_deaths <- eventReactive(input$predict_button, {
    req(input$total_cases, input$country)
    
    total_cases <- input$total_cases
    
    # Fit a simple linear regression model
    model <- lm(total_deaths ~ total_cases, data = subset(dw, location == input$country))
    
    # Check if the model fitting was successful
    if (length(model$coefficients) == 0) {
      return(list(predicted_deaths = "Model fitting failed", model_summary = ""))
    }
    
    # Get model summary
    model_summary <- summary(model)
    
    # Predict deaths based on input total cases
    predicted_deaths <- predict(model, newdata = data.frame(total_cases = total_cases))
    
    # Return predicted deaths and model summary
    return(list(predicted_deaths = round(predicted_deaths, 2), model_summary = model_summary))
  })
  
  output$prediction <- renderText({
    predict_deaths()$predicted_deaths
  })
  
  output$model_summary <- renderPrint({
    predict_deaths()$model_summary
  })
  
  output$data_plot <- renderPlot({
    ggplot(subset(dw, location == input$country), aes(x = total_cases, y = total_deaths)) +
      geom_point() +
      geom_smooth(method = "lm") +
      labs(title = "Total Cases vs Total Deaths", x = "Total Cases", y = "Total Deaths") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5))  # Center plot title
  })
  
  output$time_series_plot <- renderPlot({
    filtered_data <- dw %>%
      filter(location == input$country) %>%
      mutate(date = as.Date(date)) %>%  # Convert 'date' column to Date format
      group_by(date) %>%
      summarise(total_cases = sum(total_cases)) %>%
      na.omit()  # Remove NA values if any
    
    ggplot(filtered_data, aes(x = date, y = total_cases)) +
      geom_line() +
      labs(title = "Time Series Plot of Total Cases", x = "Date", y = "Total Cases") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5))  # Center plot title
  })
  
}
# Run the application
shinyApp(ui, server)

