# Load required libraries
library(shiny)
library(ggplot2)
library(dplyr)
library(leaflet)
library(rnaturalearth)
library(rnaturalearthdata)

# Read the CSV file
dw <- read.csv("/Users/dog/Documents/csci_6221/owid-covid-data.csv")
# Convert the date column to Date format
dw$date <- as.Date(dw$date)

unique_locations <- unique(dw$location)
metrics <- c("New Cases", "New Deaths", "Total Cases", "Total Deaths", "Vaccinated Per Hundred", "Fully Vaccinated Per Hundred")

ui <- fluidPage(
  titlePanel("COVID-19 Country-wide Analysis"),
  sidebarLayout(
    sidebarPanel(
      uiOutput("location_ui"), # Dynamic UI for locations
      selectInput("metric", "Select Metric", choices = metrics, selected = "New Cases"),
      dateInput("start_date", "Start Date:", value = "2021-01-01"),
      dateInput("end_date", "End Date:", value = "2021-12-31"),
      actionButton("add_location", "Add Another Country", icon = icon("plus")),
      actionButton("submit", "Submit")
    ),
    mainPanel(plotOutput("cases_plot"),
              leafletOutput("world_map") # Add this line for the map output
    )
  )
)


server <- function(input, output) {

  locations <- reactiveValues(list = list("United States")) # Start with 1 default location

  # country_to_iso <- unique(dw[, c("location", "iso_code")])
  # names(country_to_iso) <- country_to_iso$location
  # country_to_iso <- country_to_iso$iso_code

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
      filtered_data <- rbind(filtered_data, location_data)
    }

    # Plot the data
    output$cases_plot <- renderPlot({
      ggplot(filtered_data, aes(x = date, y = get(y_metric), color = location)) +
        geom_line() +
        labs(x = "Date", y = sprintf("Number of %s", metric),
             title = paste(metric, "in Selected Countries from", start_date, "to", end_date),
             color = "Country") +
        theme_minimal()
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
}

# Run the application
shinyApp(ui = ui, server = server)
