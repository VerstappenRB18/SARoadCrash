library(shiny)
library(leaflet)
library(sf)
library(dplyr)
library(ggplot2)
library(readr)
library(lubridate)
library(stringr)
library(plotly)
library(tidyr)
library(bslib)
library(forcats)
library(rsconnect)
options(shiny.autoreload = TRUE)

# --- Load and preprocess crash dataset ---
csv_data_raw <- read_csv("2019-2023_DATA_SA_Crash.csv")

names(csv_data_raw) <- names(csv_data_raw) |>
  str_replace_all("[^[:alnum:]]+", "_") |>
  str_to_lower()

csv_data_raw <- csv_data_raw %>%
  mutate(
    crash_date_time = str_trim(crash_date_time),
    crash_datetime = dmy_hm(crash_date_time, tz = "Australia/Adelaide"),
    crash_year = year(crash_datetime),
    crash_group = case_when(
      crash_type %in% c("Rear End", "Side Swipe", "Right Angle", "Right Turn", "Head On") ~ "Vehicle Impact", 
      crash_type %in% c("Hit Fixed Object", "Hit Object on Road", "Hit Parked Vehicle", "Hit Animal") ~ "Fixed or Other Object",
      crash_type %in% c("Roll Over", "Left Road - Out of Control") ~ "Loss of Control",
      crash_type == "Hit Pedestrian" ~ "Pedestrian",
      TRUE ~ "Other"
    ),
    stats_area = case_when(
      str_detect(stats_area, "1 City") ~ "City",
      str_detect(stats_area, "2 Metropolitan") ~ "Metropolitan",
      str_detect(stats_area, "3 Country") ~ "Country",
      TRUE ~ "Unknown"
    ),
    time_parsed = as.POSIXct(time, format = "%I:%M %p", tz = "Australia/Adelaide"),
    crash_hour = hour(time_parsed),
    time_of_day = case_when(
      crash_hour >= 5 & crash_hour < 12 ~ "Morning",
      crash_hour >= 12 & crash_hour < 17 ~ "Afternoon",
      crash_hour >= 17 & crash_hour < 21 ~ "Evening",
      TRUE ~ "Night"
    ),
    day = wday(crash_datetime, label = TRUE, abbr = TRUE, week_start = 1),
    month = month(crash_datetime, label = TRUE, abbr = TRUE),
    crash_date = as.Date(crash_datetime),
  ) %>%
  mutate(across(where(is.character), str_trim)) %>%
  filter(!is.na(accloc_x), !is.na(accloc_y))

csv_data <- st_as_sf(csv_data_raw, coords = c("accloc_x", "accloc_y"), crs = 7855) |>
  st_transform(4326)

# --- Load and preprocess casualty dataset ---
casualty_data_raw <- read_csv("2019-2023_DATA_SA_Casualty.csv")

names(casualty_data_raw) <- names(casualty_data_raw) |>
  str_replace_all("[^[:alnum:]]+", "_") |>
  str_to_lower()

# --- Load GeoJSON for Map ---
geo_data <- st_read("RoadCrashes_GDA2020.geojson")

geo_data <- geo_data %>%
  mutate(
    crash_type = case_when(
      CTY_REAR_END == 1 | CTY_SIDE_SWIPE == 1 | CTY_RIGHT_ANGLE == 1 |
        CTY_RIGHT_TURN == 1 | CTY_HEAD_ON == 1 ~ "Vehicle Impact",
      CTY_HIT_FIXED_OBJECT == 1 | CTY_HIT_OBJECT_ON_ROAD == 1 |
        CTY_HIT_PARKED_VEHILE == 1 | CTY_HIT_ANIMAL == 1 ~ "Fixed or Other Object",
      CTY_ROLL_OVER == 1 | CTY_LEFT_ROAD_OC == 1 ~ "Loss of Control",
      CTY_HIT_PEDESTRIAN == 1 ~ "Pedestrian",
      CTY_OTHER == 1 ~ "Other",
      TRUE ~ "Other" # if none match, classify as Other
    )
  )


# --- UI ---
ui <- navbarPage(
  "South Australia Road Crash Explorer",
  theme = bs_theme(bootswatch = "flatly"),
  tabPanel(
    "Map",
    sidebarLayout(
      sidebarPanel(
        width = 3,
        h4("Crash Map Filter"),
        checkboxInput("map_filter_fatal", "Only Fatal Crashes", FALSE),
        checkboxInput("map_filter_serious", "Only Serious Injury Crashes", FALSE),
        checkboxInput("map_filter_night", "Only Crashes at Night", FALSE),
        h5("Crash Types"),
        checkboxGroupInput("map_crash_type", NULL,
          choices = c(
            "Vehicle Impact",
            "Fixed or Other Object",
            "Loss of Control",
            "Pedestrian",
            "Other"
          ),
          selected = NULL
        ),
        p("Data Source: Department for Infrastructure and Transport of South Australia. (2014, July 10). Road Crash Locations in SA. Data.sa.gov.au. https://data.sa.gov.au/data/dataset/road-crashes-in-sa"),
      ),
      mainPanel(
        width = 9,
        leafletOutput("crash_map", height = "550px", width = "100%")
      )
    )
  ),
  tabPanel(
    "Crash Statistics",
    sidebarLayout(
      sidebarPanel(
        style = "max-height: 85vh",
        width = 3,
        h4("Crash Statistics Filter"),
        sliderInput("dash_year", "Select Year:",
          min = min(csv_data_raw$crash_year, na.rm = TRUE),
          max = max(csv_data_raw$crash_year, na.rm = TRUE),
          value = max(csv_data_raw$crash_year, na.rm = TRUE),
          step = 1, sep = "", animate = TRUE
        ),
        selectInput("crash_group", "Crash Type",
          choices = sort(unique(csv_data_raw$crash_group)),
          selected = NULL, multiple = TRUE
        ),
        selectInput("severity", "Severity (CSEF)",
          choices = sort(unique(csv_data_raw$csef_severity)),
          selected = NULL, multiple = TRUE
        ),
        selectInput("weather", "Weather Condition",
          choices = sort(unique(csv_data_raw$weather_cond)),
          selected = NULL, multiple = TRUE
        ),
        p("Data Source: Department for Infrastructure and Transport of South Australia. (2016, April 26). Road Crash Data. Data.sa.gov.au. https://data.sa.gov.au/data/dataset/road-crash-data"),
      ),
      mainPanel(
        width = 9,
        fluidRow(
          column(4, uiOutput("valuebox_total_crashes")),
          column(4, uiOutput("valuebox_avg_per_day")),
          column(4, uiOutput("valuebox_total_fatalities"))
        ),
        br(),
        fluidRow(
          column(6, plotlyOutput("dashboard_plot", height = "450px", width = "100%")),
          column(6, plotlyOutput("top_suburb_plot", height = "450px", width = "100%"))
        )
      )
    )
  ),
  tabPanel(
    "Casualty Statistics",
    sidebarLayout(
      sidebarPanel(
        width = 3,
        h4("Casualty Statistics Filter"),
        sliderInput("cas_year", "Select Year:",
          min = 2019, max = 2023,
          value = 2023, step = 1, sep = "", animate = TRUE
        ),
        selectInput("cas_type", "Casualty Type",
          choices = sort(unique(casualty_data_raw$casualty_type)),
          selected = NULL, multiple = TRUE
        ),
        selectInput("injury_extent", "Injury Extent",
          choices = sort(unique(casualty_data_raw$injury_extent)),
          selected = NULL, multiple = TRUE
        ),
        p("Data Source: Department for Infrastructure and Transport of South Australia. (2016, April 26). Road Crash Data. Data.sa.gov.au. https://data.sa.gov.au/data/dataset/road-crash-data"),
      ),
      mainPanel(
        width = 9,
        fluidRow(
          column(6, plotlyOutput("gender_pie", height = "300px")),
          column(6, plotlyOutput("casualty_count", height = "300px"))
        ),
        fluidRow(
          column(12, plotlyOutput("age_hist", height = "300px"))
        )
      )
    )
  )
)

# --- Server ---
server <- function(input, output, session) {
  # --- Filtered GeoJSON data for Map ---
  filtered_geo_data <- reactive({
    df <- geo_data

    if (input$map_filter_fatal) {
      df <- df[df$TOTAL_FATALITIES > 0, ]
    }

    if (input$map_filter_serious) {
      df <- df[df$CSE_SI > 0, ]
    }

    if (input$map_filter_night) {
      df <- df[df$LCO_NIGHT == 1, ]
    }

    if (!is.null(input$map_crash_type) && length(input$map_crash_type) > 0) {
      df <- df[df$crash_type %in% input$map_crash_type, ]
    }

    df
  })

  output$crash_map <- renderLeaflet({
    df <- filtered_geo_data()

    leaflet(df) %>%
      addTiles() %>%
      addCircleMarkers(
        radius = 3,
        color = ~ ifelse(CTY_HIT_PEDESTRIAN == 1, "red", "blue"),
        stroke = FALSE,
        fillOpacity = 0.7,
        popup = ~ paste0(
          "<strong>Location ID:</strong> ", UNIQUE_LOC, "<br>",
          "<strong>Total Crashes:</strong> ", TOTAL_CRASHES, "<br>",
          "<strong>Casualties:</strong> ", TOTAL_CASUALTIES, "<br>",
          "<strong>Fatalities:</strong> ", TOTAL_FATALITIES
        ),
        clusterOptions = markerClusterOptions()
      )
  })


  # --- Crash Statistics Filter ---
  filtered_csv_data <- reactive({
    df <- csv_data_raw

    df <- df[df$crash_year == input$dash_year, ]

    if (length(input$crash_group) > 0) {
      df <- df[df$crash_group %in% input$crash_group, ]
    }

    if (length(input$severity) > 0) {
      df <- df[df$csef_severity %in% input$severity, ]
    }

    if (length(input$weather) > 0) {
      df <- df[df$weather_cond %in% input$weather, ]
    }

    df
  })

  # --- Value Boxes ---
  output$valuebox_total_crashes <- renderUI({
    div(style = "background-color:#00BFC4; color:white; padding:20px; border-radius:5px;",
        h3(format(nrow(filtered_csv_data()), big.mark = ",")),
        strong("Number of Crashes")
    )
  })


  output$valuebox_avg_per_day <- renderUI({
    df <- filtered_csv_data()
    avg_per_day <- round(nrow(df) / n_distinct(df$crash_date), 0)

    div(style = "background-color:#0173B2; color:white; padding:20px; border-radius:5px;",
        h3(avg_per_day),
        strong("Average Crashes per Day")
    )
  })

  output$valuebox_total_fatalities <- renderUI({
    df <- filtered_csv_data()
    total_fatalities <- sum(df$total_fats, na.rm = TRUE)

    div(style = "background-color:#00BA38; color:white; padding:20px; border-radius:5px;",
        h3(format(total_fatalities, big.mark = ",")),
        strong("Fatalities")
    )
  })

  # --- Monthly Crashes Plot ---
  output$dashboard_plot <- renderPlotly({
    df <- filtered_csv_data()

    if (nrow(df) == 0) {
      p <- ggplot() +
        theme_void() +
        geom_text(aes(x = 1, y = 1, label = "No data to display."), size = 6) +
        theme(plot.margin = margin(30, 30, 30, 30))

      return(ggplotly(p, tooltip = NULL))
    }

    df_summary <- df %>%
      count(month, name = "count") %>%
      arrange(month)

    p <- ggplot(df_summary, aes(x = month, y = count)) +
      geom_col(fill = "#4C78A8", width = 0.7) +
      geom_text(
        aes(label = count, y = count + max(count) * 0.05),
        vjust = 0, size = 3.5, color = "black"
      ) +
      labs(
        title = paste0("Crashes by Month (", input$dash_year, ")"),
        x = NULL,
        y = NULL
      ) +
      scale_y_continuous(
        expand = expansion(mult = c(0, 0.1)),
        limits = c(0, max(df_summary$count) * 1.15)
      ) +
      theme_minimal(base_size = 10) +
      theme(
        plot.title = element_text(face = "bold", size = 10, hjust = 0, margin = margin(b = 6)),
        axis.text.x = element_text(color = "black", size = 10),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.border = element_blank()
      )

    ggplotly(p, tooltip = c("x", "y")) %>%
      layout(
        showlegend = FALSE,
        xaxis = list(title = ""),
        yaxis = list(title = "")
      )
  })


  output$top_suburb_plot <- renderPlotly({
    df <- filtered_csv_data() %>%
      filter(stats_area == "Metropolitan") %>%
      count(suburb, name = "count") %>%
      arrange(desc(count)) %>%
      slice_head(n = 10) %>%
      mutate(suburb = fct_reorder(suburb, count))

    p <- ggplot(df, aes(x = count, y = suburb)) +
      geom_col(fill = "#1F77B4") +
      geom_text(aes(label = count, x = count + max(count) * 0.05), hjust = -0.1, size = 3.5, color = "black") +
      labs(
        title = paste0("Top 10 Metropolitan Suburbs by Crashes (", input$dash_year, ")"),
        x = NULL,
        y = NULL
      ) +
      scale_x_continuous(expand = expansion(mult = c(0, 0.1))) +
      theme_minimal(base_size = 10) +
      theme(
        plot.margin = margin(0, 0, 0, 30),
        plot.title = element_text(face = "bold", size = 10, hjust = -1, margin = margin(b = 6)),
        axis.text.y = element_text(color = "black"),
        axis.text.x = element_text(color = "black"),
        axis.ticks = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
      )

    ggplotly(p, tooltip = c("x", "y")) %>%
      layout(
        xaxis = list(title = ""),
        yaxis = list(title = "")
      )
  })



  # --- Casualty Statistics Plots ---
  # Reactive subset based on selected year
  filtered_casualties <- reactive({
    df <- casualty_data_raw %>%
      mutate(
        age = as.numeric(ifelse(age %in% c("XXX", "XX"), NA, age)),
        sex = ifelse(sex %in% c("Male", "Female"), sex, "Unknown")
      ) %>%
      filter(!is.na(age), sex != "Unknown")

    # Filter by year
    df <- df[df$year == input$cas_year, ]

    # Filter by casualty type
    if (length(input$cas_type) > 0) {
      df <- df[df$casualty_type %in% input$cas_type, ]
    }

    # Filter by injury extent
    if (length(input$injury_extent) > 0) {
      df <- df[df$injury_extent %in% input$injury_extent, ]
    }

    df
  })


  # Pie chart: gender
  output$gender_pie <- renderPlotly({
    df <- filtered_casualties() %>%
      count(sex, name = "count") %>%
      complete(sex = c("Male", "Female"), fill = list(count = 0)) %>%
      mutate(sex = factor(sex, levels = c("Male", "Female"))) %>%
      arrange(sex)

    plot_ly(
      df,
      labels = ~sex,
      values = ~count,
      type = "pie",
      textinfo = "percent",
      sort = FALSE,
      marker = list(colors = c("#66C2A5", "#FC8D62"))
    ) %>%
      layout(
        title = list(
          text = paste0("Casualties by Gender (", input$cas_year, ")"),
          x = 0,
          font = list(family = "Arial Black", size = 14, color = "black")
        ),
        showlegend = TRUE,
        legend = list(orientation = "h", x = 0.25, y = -0.1),
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor = "rgba(0,0,0,0)"
      )
  })


  # Bar chart: casualty count
  output$casualty_count <- renderPlotly({
    df <- filtered_casualties() %>%
    count(report_id, name = "casualties_per_crash") %>%
    mutate(
      casualty_group = case_when(
        casualties_per_crash == 1 ~ "1",
        casualties_per_crash == 2 ~ "2",
        casualties_per_crash == 3 ~ "3",
        casualties_per_crash == 4 ~ "4",
        casualties_per_crash >= 5 ~ "5+"
      ),
      casualty_group = factor(casualty_group, levels = c("1", "2", "3", "4", "5+"))
    ) %>%
    count(casualty_group, name = "count")
    
    p <- ggplot(df, aes(x = casualty_group, y = count)) +
      geom_col(fill = "#4C78A8", width = 0.6) +
      geom_text(aes(label = count, y = count + max(count) * 0.05), size = 3.5, color = "black") +
      labs(
        title = paste0("Count of Road Crash by Casualties (", input$cas_year, ")"),
        x = "Casualties",
        y = "Number of Crashes"
      ) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
      theme_minimal(base_size = 10) +
      theme(
        plot.title = element_text(face = "bold", size = 10, hjust = -1, margin = margin(b = 6)),
        axis.text.x = element_text(color = "black"),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.border = element_blank(),
      )

    ggplotly(p, tooltip = c("x", "y")) %>%
      layout(
        showlegend = FALSE,
        xaxis = list(title = "Casualties"),
        yaxis = list(title = "Number of Crashes")
      )
  })


  # Histogram: age
  output$age_hist <- renderPlotly({
    df <- filtered_casualties()

    plot_ly(df,
      x = ~age, type = "histogram",
      marker = list(color = "#A6CEE3", line = list(width = 1, color = "white"))
    ) %>%
      layout(
        title = list(text = paste0("Age Distribution of Casualties (", input$cas_year, ")"), x = 0, font = list(family = "Arial Black", size = 14, color = "black")),
        xaxis = list(title = ""),
        yaxis = list(title = "Number of Casualties")
      )
  })
}

# --- Run the App ---
shinyApp(ui, server)