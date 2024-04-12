# Load required libraries
library(shiny)
library(ggplot2)
library(dplyr)
library(leaflet)
library(jsonlite)
library(rnaturalearth)
library(rnaturalearthdata)

# Read the CSV file
dw <- read.csv("/Users/dog/Documents/csci_6221/owid-covid-data.csv")
# Convert the date column to Date format
dw$date <- as.Date(dw$date)

unique_locations <- unique(dw$location)
metrics <- c("New Cases", "New Deaths", "Total Cases", "Total Deaths", "Vaccinated Per Hundred", "Fully Vaccinated Per Hundred", "Weekly Hospital_Admission Per Million", "Weekly ICU_Admission Per Million", "Positive Rate", "Total Test", "New Test", "Total Test Per Thousand","New Test Per Thousand")

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      .navbar { background-color: #b2d8d8 ; } /* Light Turquoise navbar */
      .navbar-nav .nav-item .nav-link { color: #ffffff; } /* White navbar text */
      body { background-color: #e9ecef;} /* Light gray background */
      .shiny-output-error { background-color: #FFF; color: #000; } /* Error message styling */
      .shiny-output-error:before { content: 'Error: '; }
      /* Additional padding for the sidebar to align with the body content */
      .well { padding-top: 40px; }
    "))
  ),
  navbarPage("COVID-19 Analysis App",
             tabPanel("Country comparator tool",
                      sidebarLayout(
                        sidebarPanel(
                          uiOutput("location_ui"), # Dynamic UI for locations
                          actionButton("add_location", "Add Another Country", icon = icon("plus")),
                          selectInput("metric", "Select Metric", choices = metrics, selected = "New Cases"),
                          dateInput("start_date", "Start Date:", value = "2021-01-01"),
                          dateInput("end_date", "End Date:", value = "2021-12-31"),
                          actionButton("submit", "Submit")
                        ),
                        mainPanel(
                          plotOutput("cases_plot"),
                          leafletOutput("world_map") # Add this line for the map output
                        )
                      )
             ),
             tabPanel("Pearsons Correlation Heatmap",
                      mainPanel(
                        plotOutput("heatmap_plot") # Heatmap output here, without sidebar
                      )
             )
  )
)

server <- function(input, output) {

  locations <- reactiveValues(list = list("United States")) # Start with 1 default location

  # Assuming dw is your dataset which includes columns 'location' for country names and 'ISO' for their ISO codes
  # Create a mapping from country names to ISO codes
  country_to_iso_df <- unique(dw[, c("location", "iso_code")])

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

  observe({
    lapply(1:length(locations$list), function(i) {
      observeEvent(input[[paste0("remove_location", i)]], {
        # Only remove the specific location associated with the remove button
        locations$list <- locations$list[-i]
      })
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
      location_data <- dw[dw$location == location & dw$date >= start_date & dw$date <= end_date, ]
      if (metric == "New Cases") {
        location_data <- location_data %>%
          filter(!is.na(new_cases) & new_cases != 0)
      }
      if (metric == "New Deaths") {
        location_data <- location_data %>%
          filter(!is.na(new_cases) & new_cases != 0)
      }
      if (metric == "Vaccinated Per Hundred") {
        location_data <- location_data %>%
          filter(!is.na(new_cases) & new_cases != 0)
      }
      if (metric == "Fully Vaccinated Per Hundred") {
        location_data <- location_data %>%
          filter(!is.na(new_cases) & new_cases != 0)
      }
      if (metric == "Weekly Hospital_Admission Per Million") {
        location_data <- location_data %>%
          filter(!is.na(new_cases) & new_cases != 0)
      }
      if (metric == "Weekly ICU_Admission Per Million") {
        location_data <- location_data %>%
          filter(!is.na(new_cases) & new_cases != 0)
      }
      if (metric == "Positive Rate") {
        location_data <- location_data %>%
          filter(!is.na(new_cases) & new_cases != 0)
      }
      if (metric == "Total Test") {
        location_data <- location_data %>%
          filter(!is.na(new_cases) & new_cases != 0)
      }
      if (metric == "New Test") {
        location_data <- location_data %>%
          filter(!is.na(new_cases) & new_cases != 0)
      }
      if (metric == "Total Test Per Thousand") {
        location_data <- location_data %>%
          filter(!is.na(new_cases) & new_cases != 0)
      }
      if (metric == "New Test Per Thousand") {
        location_data <- location_data %>%
          filter(!is.na(new_cases) & new_cases != 0)
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
    date_filtered_data <- dw %>% filter(date == as.Date("2023-01-01"))

    # Select the columns of interest for rows and columns in the heatmap
    row_variables <- date_filtered_data %>%
      select(total_deaths_per_million, total_cases_per_million,
             weekly_icu_admissions_per_million, weekly_hosp_admissions_per_million)

    col_variables <- date_filtered_data %>%
      select(people_fully_vaccinated_per_hundred, people_vaccinated_per_hundred,
             total_boosters_per_hundred, population_density,
             median_age)

    # Compute Pearson's correlation
    correlation_matrix <- cor(row_variables, col_variables, use = "pairwise.complete.obs", method = "pearson")

    # Convert the correlation matrix to a format suitable for ggplot2
    correlation_data <- as.data.frame(as.table(correlation_matrix))

    names(correlation_data) <- c("RowVariable", "ColVariable", "Correlation")

    # Generate the heatmap
    ggplot(correlation_data, aes(x = RowVariable, y = ColVariable, fill = Correlation)) +
      geom_tile() +
      # scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0) +
      scale_fill_gradient2(low = "skyblue", high = "darkred", mid = "white", midpoint = 0) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      labs(title = "Pearson's Correlation Heatmap on 01/01/2023", x = "", y = "") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12, face = "bold"),  # Bold and larger x-axis text
            axis.text.y = element_text(size = 12, face = "bold"),  # Bold and larger y-axis text
            axis.title.x = element_text(size = 14, face = "bold"),  # Bold and larger x-axis title
            axis.title.y = element_text(size = 14, face = "bold"),  # Bold and larger y-axis title
            plot.title = element_text(size = 16, face = "bold", hjust = 0.5),  # Bold and larger plot title, centered
            legend.title = element_text(size = 12, face = "bold"),  # Bold and larger legend title
            legend.text = element_text(size = 12, face = "bold"))  # Bold and larger legend text
  })

}

# Run the application
shinyApp(ui = ui, server = server)

