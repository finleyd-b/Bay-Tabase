# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

# Load packages
library(shiny)
library(dplyr)
library(lubridate)
library(tidyverse)
library(plotly)
library(DT)
library(shinyWidgets)
library(leaflet)

# Load and rename the CSV files
# File names can't start with numbers, so we write "X..."
data1 <- read_csv("Data/1969_2015SanFranciscoBayWaterQualityData.csv")
data2 <- read_csv("Data/2016_2021SanFranciscoBayWaterQualityData.csv")
stations_data <- read_csv("Data/CurrentStationData.csv")
phytoplankton1 <- read_csv("Data/Phytoplankton_San_Francisco_Bay_1992_2014.csv", locale = locale(encoding = "latin1"))
phytoplankton2 <- read_csv("Data/Phytoplankton_San_Francisco_Bay_2014_2016.csv",  locale = locale(encoding = "latin1"))
phytoplankton3 <- read_csv("Data/Phytoplankton_San_Francisco_Bay_2017_2018.csv",  locale = locale(encoding = "latin1"))

# Combine the two general data files and three phytoplankton data files
combined_data <- bind_rows(data1, data2)
combined_phytoplankton_data <- bind_rows(phytoplankton1, phytoplankton2, phytoplankton3)

# Cleaning
combined_phytoplankton_data <- combined_phytoplankton_data |>
  mutate(
    Date = mdy(Date),
    year = year(Date)
  )

# Define the variable labels
variable_labels <- c(
  Temperature = "Temperature (°C)",
  Salinity = "Salinity (PSU)",
  Depth = "Depth (m)",
  Oxygen = "Oxygen (mg/L)"
)

# Reformat the combined_data date column to mm/dd/yyyy
combined_data$Date <- mdy(combined_data$Date)

# Remove variable columns that I'm not using
combined_data <- combined_data |>
  select(-"Calculated_Oxygen", -"Oxygen_Percent_Saturation", -"Julian_Date")

# Tidy the stations_data
stations_data <- stations_data[rowSums(is.na(stations_data)) != ncol(stations_data), ]
stations_data <- stations_data |>
  rename(
    "Latitude" = "Latitude (degrees)",
    "Longitude" = "Longitude (degrees)"
  )

# Define colorblind-friendly palette (Okabe & Ito)
cbPalette <- c(
  blue = "#0072B2",
  green = "#009E73",
  orange = "#E69F00",
  red = "#D55E00",
  purple = "#CC79A7"
)

# Define nicer names for variables
var_choices <- c(
  "Temperature (°C)" = "Temperature",
  "Salinity (PSU)" = "Salinity",
  "Discrete Chlorophyll (µg/L)" = "Discrete_Chlorophyll",
  "Oxygen (mg/L)" = "Oxygen"
)

#-------------------------------------------------------------------------------

ui <- fluidPage(
  #Styling block
  tags$style(HTML("
    body {
      background-color: #0072B2;
      color: white;
    }
    
    .tab-content {
      background-color: #0072B2;
    }
    
    /* Make all tabs readable*/
    .nav-tabs > li > a {
    color: white !important;
    background-color: #0072B2 !important;
    border-color: white !important;
    }
    
    /* Hover */
  .nav-tabs > li > a:hover {
    color: white !important;
    background-color: #005b8c !important;
  }

/* Active tab */
  .nav-tabs > li.active > a,
  .nav-tabs > li.active > a:hover,
  .nav-tabs > li.active > a:focus {
    color: white !important;
    background-color: #005b8c !important;
    border-color: white !important;
  }
  ")),
  
  titlePanel(
    div(
      style = "text-align:center;", 
      HTML("<div style='font-size:60px; font-weight:bold; line-height:1; text-decoration:underline;'>Bay-tabase:</div>
         <div style='font-size:15px; line-height:2; margin-top:-10;'>50 Years of San Francisco Bay Water Quality</div>")
    )
  ),
  
  # Centering tabs and their content on the page
  tags$head(
    tags$style(HTML("
    .nav.nav-tabs {
      display: flex !important;
      justify-content: center !important;
    }
  "))
  ),
  
  #-------------------------------------------------------------------------------
  
  tabsetPanel(
    tabPanel("Helpful Information",
             div(
               style = "text-align:center; max-width:900px; margin:0 auto",
               
               strong("About San Francisco Bay:", style = "font-size:22px; margin-top:20px; display:block; text-decoration:underline;"),
               p("San Francisco Bay is the largest estuary on the west coast of North America and represents a dynamic environment where freshwater from rivers mixes with seawater from the Pacific Ocean. This mixing creates strong gradients in salinity, nutrients, and biological productivity. Monitoring water quality in the bay helps scientists understand how environmental factors such as climate variability, freshwater inflow, and human activity influence the ecosystem.", style = "font-size:16px"),
               
               strong("About the Monitoring Program:", style = "font-size:22px; margin-top:20px; display:block; text-decoration:underline;"),
               p("The data used in this application come from long-term monitoring efforts conducted by the U.S. Geological Survey (USGS). These programs measure environmental conditions such as temperature, salinity, oxygen, nutrients, and phytoplankton indicators to better understand the ecological health of the San Francisco Bay estuary. Long-term monitoring is essential for detecting environmental change, understanding ecosystem dynamics, and informing coastal management decisions.", style = "font-size:16px"),
               
               strong("Data Sourcing:", style = "font-size:22px; margin-top:20px; display:block; text-decoration:underline;"),
               p("This app visualizes water quality data collected by the United States Geological Survey in San Francisco Bay between 1969 and 2023. The dataset contains long-term measurements of physical and chemical properties of the bay's water column, allowing users to explore trends over time, depth profiles, and spatial patterns across monitoring stations.", style = "font-size:16px"),
               
               strong("Using the App:", style = "font-size:22px; margin-top:20px; display:block; text-decoration:underline;"),
               p("The tabs above allow you to explore the dataset in several ways:", style = "font-size:16px"),
               p(strong("Station Map:"), " View monitoring locations throughout San Francisco Bay and click on a station to display the full dataset associated with that location.", style = "font-size:16px"),
               p(strong("Long-Term Trends:"), " Examine how key water quality variables have changed over time across the bay.", style = "font-size:16px"),
               p(strong("Depth Profiles:"), " Explore how variables such as temperature, salinity, or oxygen vary with depth at a selected station and year.", style = "font-size:16px"),
               p(strong("Exploratory Plots:"), " Investigate relationships between different water quality variables.", style = "font-size:16px"),
               p(strong("Phytoplankton Plots:"), " Explore phytoplankton community composition and abundance trends over time by taxa.", style = "font-size:16px"),
               
               strong("Data Notes:", style = "font-size:22px; margin-top:20px; display:block; text-decoration:underline;"),
               p("Not all stations were sampled during every year, and some measurements may be missing due to weather conditions, equipment issues, or quality control procedures. As a result, some plots may contain gaps or fewer observations for certain stations or time periods.", style = "font-size:16px"),
               
               strong("Understanding the Data:", style = "font-size:22px; margin-top:20px; display:block; text-decoration:underline;"),
               p("Use this glossary as a reference tool if you have any questions pertaining a certain variable!", style = "font-size:16px"),
               p(strong("Temperature (°C):"), " Water temperature measured in degrees Celsius.", style = "font-size:16px"),
               p(strong("Salinity (PSU):"), " The salt concentration of water measured in practical salinity units.", style = "font-size:16px"),
               p(strong("Depth (m):"), " Depth below the water surface measured in meters.", style = "font-size:16px"),
               p(strong("Oxygen (mg/L):"), " Concentration of dissolved oxygen in milligrams per liter.", style = "font-size:16px"),
               p(strong("Discrete Chlorophyll (µg/L):"), " Concentration of chlorophyll pigment indicating phytoplankton biomass.", style = "font-size:16px"),
               p(strong("Extinction Coefficient (1/m):"), " Measure of light attenuation in the water column.", style = "font-size:16px"),
               p(strong("Sigma-t:"), " Density anomaly used in oceanography to identify water masses.", style = "font-size:16px"),
               p(strong("Discrete SPM (mg/L):"), " Suspended particulate matter concentration in milligrams per liter.", style = "font-size:16px"),
               p(strong("Calculated Chlorophyll (µg/L):"), " Chlorophyll concentration estimated from fluorescence measurements.", style = "font-size:16px"),
               p(strong("Chlorophyll-Pheopigment Ratio:"), " Ratio indicating chlorophyll degradation products.", style = "font-size:16px"),
               p(strong("NO2 (µM):"), " Nitrite concentration measured in micromoles per liter.", style = "font-size:16px"),
               p(strong("NO32 (µM):"), " Combined nitrate and nitrite concentration in micromoles per liter.", style = "font-size:16px"),
               p(strong("NH4 (µM):"), " Ammonium concentration in micromoles per liter.", style = "font-size:16px"),
               p(strong("PO4 (µM):"), " Phosphate concentration in micromoles per liter.", style = "font-size:16px"),
               p(strong("Si (µM):"), " Silicate concentration in micromoles per liter.", style = "font-size:16px"),
               p(strong("Taxonomic Identification:"), " The scientific name or group assigned to the observed phytoplankton organism.", style = "font-size:16px"),
               p(strong("Phylum or Class:"), " The broader taxonomic grouping (phylum or class) to which the identified organism belongs.", style = "font-size:16px"),
               p(strong("Station Number:"), " The numeric identifier of the monitoring station where the phytoplankton sample was collected.", style = "font-size:16px"),
               p(strong("Density (cells/mL):"), " The number of phytoplankton cells per milliliter of water, indicating organism abundance.", style = "font-size:16px"),
               p(strong("Biovolume (cubic micrometers/mL):"), " The total volume of phytoplankton cells per milliliter, used as a proxy for biomass.", style = "font-size:16px"),
               p(strong("Cell Volume (cubic micrometers/cell):"), " The average volume of a single phytoplankton cell, reflecting cell size.", style = "font-size:16px"),
               p(strong("Actual Count:"), " The raw number of phytoplankton cells counted in the laboratory sample before density calculations.", style = "font-size:16px"),
               
               #Adding photos
               div(
                 style = "display: flex; justify-content: center; gap: 20px; margin-top:30px;",
                 img(src = "Logo.jpg", height = "200px", style = "border-radius:8px;"),
                 img(src = "Boat.jpg", height = "200px", style = "border-radius:8px;")
               )
             )
    ),
    
    tabPanel("Station Map",
             sidebarLayout(
               sidebarPanel(
                 helpText("Click a station marker to view its data below."),
                 width = 3,
                 style = "color: #495057"
               ),
               mainPanel(
                 # Blurb now sits to the right of the sidebar, at the top of the main panel
                 p("Explore the locations of long-term water quality monitoring stations throughout San Francisco Bay.
                   Click on any station marker to view the full dataset collected at that location, including measurements
                   of temperature, salinity, oxygen, nutrients, and more.",
                   style = "margin-top:10px; margin-bottom:10px;"),
                 leafletOutput("stationMap", height = "600px"),
                 width = 9
               )
             ),
             
             conditionalPanel(
               condition = "input.stationMap_marker_click != null",
               
               div(
                 style = "height:400px; overflow-y:auto; margin-top:15px; width:100%; padding:10px; background-color:white; color:black; border-radius:6px;",
                 
                 strong(textOutput("stationTitle"), style = "font-size:20px;"),
                 
                 DTOutput("stationTable")
               )
             )
    ),
    
    tabPanel("Long-Term Trends",
             sidebarLayout(
               sidebarPanel(
                 width = 3,
                 style = "color: #495057",
                 selectInput("overview_var", "Select Variable:", choices = var_choices)
               ),
               mainPanel(
                 p("Overview of key water quality variables and long-term trends.",
                   style = "margin-top:10px"
                 ),
                 plotOutput("overviewPlot", height = "700px")
               )
             )
    ),
    
    tabPanel("Depth Profiles",
             sidebarLayout(
               sidebarPanel(
                 width = 3,
                 style = "color: #495057",
                 selectInput("depth_var", "Variable:", choices = var_choices),
                 selectInput("depth_station", "Station:", choices = NULL),
                 selectInput("depth_year", "Year:", choices = NULL),
                 checkboxInput("show_smooth", "Show smooth trend line", value = TRUE),
                 tags$small("* Some years may be unavailable due to lack of sampling, equipment issues, or quality control exclusions.")
               ),
               mainPanel(
                 p("Visualize depth profiles of selected water quality variables.",
                   style = "margin-top:10px"
                 ),
                 plotOutput("depthPlot", height = "700px")
               )
             )
    ),
    
    tabPanel("Exploratory Plots",
             sidebarLayout(
               sidebarPanel(
                 width = 3,
                 style = "color: #495057",
                 selectInput("x_var", "X Variable:", choices = NULL),
                 selectInput("y_var", "Y Variable:", choices = NULL)
               ),
               mainPanel(
                 p("Explore relationships between different water quality variables.",
                   style = "margin-top:10px"
                 ),
                 plotOutput("scatterPlot", height = "700px")
               )
             )
    ),
    
    tabPanel("Phytoplankton Plots",
             
             sidebarLayout(
               sidebarPanel(
                 width = 3,
                 style = "color: #495057",
                 # Multi-selection drop-down for species
                 pickerInput(
                   inputId = "taxaInput",
                   label = "Select Taxa:",
                   choices = sort(unique(combined_phytoplankton_data$`Taxonomic Identification`)),
                   selected = sort(unique(combined_phytoplankton_data$`Taxonomic Identification`))[1],
                   multiple = TRUE,
                   options = list(`actions-box` = TRUE)
                 ),
                 # Year range slider
                 sliderInput(
                   inputId = "yearRange",
                   label = "Select Year Range:",
                   min = min(lubridate::year(combined_phytoplankton_data$Date), na.rm = TRUE),
                   max = max(lubridate::year(combined_phytoplankton_data$Date), na.rm = TRUE),
                   value = c(min(lubridate::year(combined_phytoplankton_data$Date), na.rm = TRUE),
                             max(lubridate::year(combined_phytoplankton_data$Date), na.rm = TRUE)),
                   step = 1,
                   sep = ""
                 )
               ),
               
               mainPanel(
                 p("Explore patterns in phytoplankton communities over time.
                   Use the selector to choose one or more taxa and a year range to compare
                   abundance trends and average density across groups.",
                   style = "margin-top:10px; margin-bottom:15px;"),
                 tabsetPanel(
                   tabPanel("Time Series",
                            plotlyOutput("phytoTimeSeries", height = "600px")
                   ),
                   tabPanel("Summary Stats",
                            plotlyOutput("phytoSummary", height = "600px")
                   )
                 )
               )
             )
    )
  )
)
#-------------------------------------------------------------------------------

server <- function(input, output, session) {
  output$stationMap <- renderLeaflet({
    leaflet(data = stations_data) |> 
      addTiles() |> 
      addCircleMarkers(
        lng = ~Longitude,
        lat = ~Latitude,
        layerId = ~as.character(Station),
        label = lapply(stations_data$Station, function(station) {
          htmltools::HTML(
            paste0(
              "<div style='
              background-color: rgba(255, 255, 255, 0.8);
              padding: 4px 8px;
              border-radius: 4px;
              font-weight: bold;
              color: black;
              font-size: 14px;'>",
              "Station ", station,
              "</div>"
            )
          )
        }),
        color = "#E69F00",
        radius = 6,
        stroke = FALSE,
        fillOpacity = 0.9
      )
  })
  
  selected_station <- reactiveVal(NULL)
  
  observeEvent(input$stationMap_marker_click, {
    selected_station(input$stationMap_marker_click$id)
  })
  
  output$stationTable <- renderDT({
    req(selected_station())
    
    combined_data |>
      filter(Station_Number == as.numeric(selected_station())) |>
      select(-DiscreteChlorophyll_Pheopigment_Ratio) |>      # Remove this column
      mutate(Date = format(Date, "%m/%d/%Y")) |>              # Format Date
      select(Date, everything()) |>                            # Put Date first
      datatable(options = list(pageLength = 10), rownames = FALSE)
  })
  
  observe({
    stations <- unique(combined_data$Station_Number)
    choices_named <- setNames(stations, paste("Station", stations))
    updateSelectInput(session, "depth_station", choices = choices_named)
  })
  
  observeEvent(input$depth_station, {
    req(input$depth_station)
    available_years <- combined_data %>%
      filter(Station_Number == input$depth_station) %>%
      mutate(Year = year(Date)) %>%
      distinct(Year) %>%
      arrange(Year) %>%
      pull(Year)
    updateSelectInput(session, "depth_year", choices = available_years, selected = available_years[1])
  })
  
  observe({
    vars <- names(combined_data) |> discard(~ .x %in% c("Date", "Time", "Station", "Latitude", "Longitude"))
    
    pretty_labels <- c(
      "Temperature (°C)" = "Temperature",
      "Salinity (PSU)" = "Salinity",
      "Discrete Chlorophyll (µg/L)" = "Discrete_Chlorophyll",
      "Oxygen (mg/L)" = "Oxygen",
      "Extinction Coefficient (1/m)" = "Extinction_Coefficient",
      "Sigma-t" = "Sigma_t",
      "Discrete SPM (mg/L)" = "Discrete_SPM",
      "Calculated Chlorophyll (µg/L)" = "Calculated_Chlorophyll",
      "Chlorophyll-Pheopigment Ratio" = "Chl_Pheo_Ratio",
      "NO2 (µM)" = "NO2",
      "NO32 (µM)" = "NO32",
      "NH4 (µM)" = "NH4",
      "PO4 (µM)" = "PO4",
      "Si (µM)" = "Si"
    )
    
    available <- intersect(pretty_labels, vars)
    final_choices <- setNames(available, names(pretty_labels)[match(available, pretty_labels)])
    
    updateSelectInput(session, "x_var", choices = final_choices, selected = available[1])
    updateSelectInput(session, "y_var", choices = final_choices, selected = available[2])
  })
  
  plot_theme <- theme_minimal(base_size = 18)
  
  output$overviewPlot <- renderPlot({
    req(input$overview_var)
    var <- sym(input$overview_var)
    combined_data |>
      group_by(Date) |>
      summarise(value = mean(!!var, na.rm = TRUE)) |>
      ggplot(aes(x = Date, y = value)) +
      geom_line(color = cbPalette["blue"]) +
      labs(title = paste("Long-term trend of", input$overview_var), x = "Year", y = input$overview_var) +
      plot_theme
  })
  
  output$depthPlot <- renderPlot({
    req(input$depth_var, input$depth_station, input$depth_year)
    
    var <- sym(input$depth_var)
    
    # Axis label mapping
    label_map <- c(
      "Temperature" = "Temperature (°C)",
      "Salinity" = "Salinity (PSU)",
      "Discrete_Chlorophyll" = "Discrete Chlorophyll (µg/L)",
      "Oxygen" = "Oxygen (mg/L)"
    )
    
    x_label <- label_map[[input$depth_var]] %||% input$depth_var
    
    p <- combined_data |>
      filter(Station_Number == input$depth_station, year(Date) == input$depth_year) |>
      ggplot(aes(x = !!var, y = Depth)) +
      geom_point(color = cbPalette["green"]) +
      scale_y_reverse() +
      labs(
        title = paste("Depth profile of", x_label, "at Station", input$depth_station, "in", input$depth_year),
        x = x_label, y = "Depth (m)"
      ) +
      plot_theme
    
    if (isTRUE(input$show_smooth)) {
      p <- p + geom_smooth(color = "#CC79A7", method = "loess", se = FALSE)
    }
    
    p
  })
  
  output$scatterPlot <- renderPlot({
    req(input$x_var, input$y_var)
    
    xvar <- sym(input$x_var)
    yvar <- sym(input$y_var)
    
    label_map <- c(
      "Temperature" = "Temperature (°C)",
      "Salinity" = "Salinity (PSU)",
      "Discrete_Chlorophyll" = "Discrete Chlorophyll (µg/L)",
      "Oxygen" = "Oxygen (mg/L)",
      "Extinction_Coefficient" = "Extinction Coefficient (1/m)",
      "Sigma_t" = "Sigma-t",
      "Discrete_SPM" = "Discrete SPM (mg/L)",
      "Calculated_Chlorophyll" = "Calculated Chlorophyll (µg/L)",
      "Chl_Pheo_Ratio" = "Chlorophyll-Pheopigment Ratio",
      "NO2" = "NO2 (µM)",
      "NO32" = "NO32 (µM)",
      "NH4" = "NH4 (µM)",
      "PO4" = "PO4 (µM)",
      "Si" = "Si (µM)"
    )
    
    x_label <- label_map[[input$x_var]] %||% input$x_var
    y_label <- label_map[[input$y_var]] %||% input$y_var
    
    combined_data |>
      ggplot(aes(x = !!xvar, y = !!yvar)) +
      geom_point(alpha = 0.5, color = cbPalette["orange"]) +
      labs(
        title = paste("Relationship between", x_label, "and", y_label),
        x = x_label,
        y = y_label
      ) +
      theme_minimal(base_size = 16) +
      theme(
        plot.title = element_text(size = 20, face = "bold"),
        axis.title = element_text(size = 18),
        axis.text = element_text(size = 14)
      )
  })
  
  # Phytoplankton Plots
  # Reactive filtered data based on user input
  filtered_phyto <- reactive({
    req(input$taxaInput, input$yearRange)
    
    combined_phytoplankton_data |>
      mutate(year = lubridate::year(Date)) |>
      filter(
        `Taxonomic Identification` %in% input$taxaInput,
        year >= input$yearRange[1],
        year <= input$yearRange[2]
      )
  })
  
  # Time Series Plot
  output$phytoTimeSeries <- renderPlotly({
    df <- filtered_phyto() |>
      group_by(year, `Taxonomic Identification`) |>
      summarise(`Density (cells/mL)` = mean(`Density (cells/mL)`, na.rm = TRUE))
    
    plot_ly(df, x = ~year, y = ~`Density (cells/mL)`, color = ~`Taxonomic Identification`,
            type = 'scatter', mode = 'lines+markers') |>
      layout(title = "Phytoplankton Abundance Over Time",
             xaxis = list(title = "Year"),
             yaxis = list(title = "Average Abundance (cells/mL)"))
  })
  
  # Summary Stats Plot
  output$phytoSummary <- renderPlotly({
    df <- filtered_phyto() |>
      group_by(`Taxonomic Identification`, year) |>
      summarise(mean_density = mean(`Density (cells/mL)`, na.rm = TRUE), .groups = "drop")
    
    plot_ly(df,
            x = ~`Taxonomic Identification`,
            y = ~year,
            z = ~mean_density,
            type = "scatter3d",
            mode = "markers",
            marker = list(size = 5),
            color = ~`Taxonomic Identification`) |>
      layout(
        title = "Average Abundance by Taxa and Year",
        scene = list(
          xaxis = list(title = "Taxa"),
          yaxis = list(title = "Year"),
          zaxis = list(title = "Mean Abundance (cells/mL)")
        )
      )
  })
}

shinyApp(ui = ui, server = server)