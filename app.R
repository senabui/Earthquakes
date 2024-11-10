library(shiny)
library(leaflet)
library(sf)
library(dplyr)
library(RColorBrewer)
library(fmsb)
library(plotly)
library(ggplot2)
library(DT)
library(tidyr)
library(reshape2)

ui <- fluidPage(
  titlePanel(
    title = "Global Earthquake Visualization"
  ),
  
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput("alert_levels",
                         "Select Alert Levels:",
                         choices = c("Yellow", "Orange", "Red"),
                         selected = c("Yellow", "Orange", "Red")),
      
      sliderInput("magnitude",
                  "Earthquake Magnitude:",
                  min = 0,
                  max = 10,
                  value = c(0, 10),
                  step = 0.1),
      
      sliderInput("population",
                  "City Population (millions):",
                  min = 0,
                  max = 40,
                  value = c(2, 40),
                  step = 0.1),
      
      checkboxInput("show_plates", 
                    "Show Tectonic Plates", 
                    value = TRUE),
      
      checkboxInput("highlightTsunami", "Highlight Tsunamis", FALSE),
      
      hr(),
      helpText("Click on an earthquake to see detailed metrics below:"),
      plotOutput("starPlot", height = "300px")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Map", 
                 leafletOutput("map", height = "800px")
        ),
        tabPanel("Time",
                 plotlyOutput("yearTrendPlot", height = "600px")
        ),
        tabPanel("Continent",
                 fluidRow(
                   column(12,
                          selectInput("selectedContinent", 
                                      "Select Continent:",
                                      choices = c("North America", "South America", "Europe", 
                                                  "Asia", "Africa", "Oceania"),
                                      selected = "North America")
                   )
                 ),
                 fluidRow(
                   column(12,
                          plotlyOutput("continentMetrics", height = "600px")
                   )
                 ),
                 fluidRow(
                   column(12,
                          dataTableOutput("continentSummary")
                   )
                 )
        ),
        tabPanel("Country Comparison",
                 fluidRow(
                   column(6,
                          selectInput("continent1", 
                                      "Select First Continent:",
                                      choices = c("North America", "South America", "Europe", 
                                                  "Asia", "Africa", "Oceania"))
                   ),
                   column(6,
                          selectInput("continent2", 
                                      "Select Second Continent:",
                                      choices = c("North America", "South America", "Europe", 
                                                  "Asia", "Africa", "Oceania"))
                   )
                 ),
                 plotlyOutput("countryComparisonPlot", height = "600px")
        ),
        tabPanel("Tsunami Analysis",
                 fluidRow(
                   column(6, plotlyOutput("tsunamiMagnitudeBox")),
                   column(6, plotlyOutput("tsunamiDepthBox"))
                 )
        )
      ),
      tags$head(
        tags$style(HTML("
            .loading-spinner {
                position: absolute;
                right: 20px;
                top: 20px;
                z-index: 1000;
            }
        "))
      ),
      
      # Add notification area
      div(id = "notification-area", class = "notification-area"),
      
      # ... rest of existing UI code ...
    )
  ),
  
  # Add custom message at the bottom of the page
  tags$div(
    style = "position: fixed; bottom: 0; width: 100%; text-align: center; padding: 10px; background-color: #f9f9f9;",
    "Data Insighters: Charlotte A., Sena B., Trisha G., Om D., Helly S., Kapil K."
  )
)


server <- function(input, output, session) {
  selected_quake <- reactiveVal(NULL)
  selected_cities <- reactiveVal(c())
  
  plates <- reactive({
    plates_data <- st_read("Data/tectonicplates-master/PB2002_plates.shp", quiet = TRUE)
    return(plates_data)
  })
  
  earthquakes <- reactive({
    eq_data <- read.csv("Data/modified_earthquake_data.csv") %>%
      mutate(
        alert_new = tolower(alert_new),
        magnitude = as.numeric(magnitude)
      ) %>%
      filter(
        !is.na(magnitude),
        magnitude >= input$magnitude[1],
        magnitude <= input$magnitude[2],
        alert_new %in% tolower(input$alert_levels)
      )
    
    eq_data$marker_color <- case_when(
      eq_data$alert_new == "yellow" ~ "#FFd800",
      eq_data$alert_new == "orange" ~ "#FF6600",
      eq_data$alert_new == "red" ~ "#BF0d0d",
      TRUE ~ "#000000"
    )
    
    return(eq_data)
  })
  
  cities <- reactive({
    cities_data <- read.csv("Data/worldcities.csv") %>%
      mutate(
        population_m = population / 1000000,
        lng = as.numeric(lng),
        lat = as.numeric(lat),
        city_id = row_number()  # Add unique ID for each city
      ) %>%
      filter(
        population_m >= input$population[1],
        population_m <= input$population[2],
        !is.na(lng) & !is.na(lat)
      )
    return(cities_data)
  })
  
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles(
        urlTemplate = 'https://{s}.basemaps.cartocdn.com/light_all/{z}/{x}/{y}{r}.png', 
        attribution = '&copy; <a href="https://www.openstreetmap.org/copyright">OpenStreetMap</a> contributors &copy; <a href="https://carto.com/attributions">CARTO</a>',
        options = tileOptions(subdomains = 'abcd', maxZoom = 20)
      ) %>%
      setView(lng = 0, lat = 0, zoom = 2) %>%
      addScaleBar(position = "bottomleft")
  })
  
  
  # Star plot for earthquake details
  output$starPlot <- renderPlot({
    quake <- selected_quake()
    if (!is.null(quake)) {
      max_min <- data.frame(
        Magnitude = c(10, 0),
        Depth = c(max(earthquake_data$depth, na.rm = TRUE), 0),
        MMI = c(12, 0),
        CDI = c(12, 0),
        Significance = c(max(earthquake_data$sig, na.rm = TRUE), 0)
      )
      
      alert_value <- case_when(
        quake$alert_new == "red" ~ 3,
        quake$alert_new == "orange" ~ 2,
        quake$alert_new == "yellow" ~ 1,
        TRUE ~ 0
      )
      
      quake_data <- data.frame(
        Magnitude = as.numeric(quake$magnitude),
        Depth = as.numeric(quake$depth),
        MMI = as.numeric(quake$mmi),
        CDI = as.numeric(quake$cdi),
        Significance = as.numeric(quake$sig)
      )
      
      plot_data <- rbind(max_min, quake_data)
      
      par(bg = "white")
      radarchart(
        plot_data,
        pcol = quake$marker_color,
        pfcol = scales::alpha(quake$marker_color, 0.5),
        plwd = 2,
        cglcol = "grey",
        cglty = 1,
        axislabcol = "grey50",
        vlcex = 0.8,
        title = "Earthquake Metrics"
      )
    } else {
      plot(NULL, xlim=c(0,1), ylim=c(0,1), 
           xlab="", ylab="", 
           axes=FALSE)
      text(0.5, 0.5, "Click an earthquake to view metrics", cex=1.2)
    }
  })
  
  observe({
    req(input$alert_levels, plates(), earthquakes(), cities())
    
    eq_data <- earthquakes()
    plate_data <- plates()
    city_data <- cities()
    selected_city_ids <- selected_cities()
    
    alert_colors <- c(
      "yellow" = "#FFd800",
      "orange" = "#FF6600",
      "red" = "#BF0d0d"
    )
    
    legend_html <- paste(
      "<span style='background-color:", alert_colors, "; width: 10px; height: 10px; display: inline-block;'></span>",
      toupper(names(alert_colors)),
      "<br>"
    )
    
    map <- leafletProxy("map") %>%
      clearMarkers() %>%
      clearShapes() %>%
      clearControls()
    
    if(input$show_plates) {
      map <- map %>%
        addPolygons(
          data = plate_data,
          weight = 2,
          color = "#444444",
          fillOpacity = 0.1,
          popup = ~PlateName,
          group = "Tectonic Plates"
        )
    }
    
    if(input$highlightTsunami) {
      tsunami_data <- eq_data %>% filter(tsunami == 1)
      map <- map %>% 
        addCircleMarkers(
          data = tsunami_data,
          lat = ~latitude,
          lng = ~longitude,
          radius = ~magnitude * 2.5,
          color = "#1d55d3",
          weight = 2,
          fillColor = NA,
          fillOpacity = 0,
          opacity = 0.8,
          group = "Tsunami Rings"
        )
    }
    
    map <- map %>%
      addCircleMarkers(
        data = eq_data,
        lng = ~longitude,
        lat = ~latitude,
        radius = ~pmin(magnitude * 2, 20),
        color = ~marker_color,
        fillColor = ~marker_color,
        fillOpacity = 0.7,
        stroke = TRUE,
        weight = 1,
        label = ~paste(
          "Name:",title,
          "Magnitude:", magnitude,
          "| Depth:", depth, "km",
          "| Date:", year,
          "| Alert:", toupper(alert_new)
        ),
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto"
        ),
        popup = ~paste(
          "<strong>Earthquake Details</strong><br>",
          "Name:",title,"<br>",
          "Magnitude:", magnitude, "<br>",
          "Depth:", depth, "km<br>",
          "Date:", year, "<br>",
          "Alert Level:", toupper(alert_new)
        ),
        group = "Earthquakes",
        layerId = ~paste("eq", row.names(eq_data))
      )
    
    
    
    map <- map %>%
      addCircleMarkers(
        data = city_data,
        lng = ~lng,
        lat = ~lat,
        radius = ~pmin(sqrt(population_m) * 2, 15),
        color = "#444d4e",
        fillColor = "#444d4e",
        fillOpacity = 0.4,
        stroke = TRUE,
        weight = 1,
        label = ~paste(city, "-", format(population, big.mark = ","), "people"),
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto"
        ),
        group = "Major Cities",
        layerId = ~paste("city", city_id)
      )
    
    if(length(selected_city_ids) > 0) {
      selected_cities_data <- city_data %>%
        filter(city_id %in% selected_city_ids)
      
      map <- map %>%
        addLabelOnlyMarkers(
          data = selected_cities_data,
          lng = ~lng,
          lat = ~lat,
          label = ~paste(city, "-", format(population, big.mark = ",")),
          labelOptions = labelOptions(
            noHide = TRUE,
            direction = "auto",
            textsize = "12px",
            style = list(
              "background-color" = "white",
              "border-color" = "black",
              "border-width" = "1px",
              "border-style" = "solid",
              "padding" = "5px"
            )
          ),
          group = "City Labels"
        )
    }
    
    map %>%
      addControl(
        html = paste(
          "<div style='background-color: white; padding: 10px; border-radius: 5px;'>",
          "<strong>Alert Levels</strong><br>",
          paste(legend_html, collapse = ""),
          "</div>"
        ),
        position = "bottomright"
      ) %>%
      addLayersControl(
        overlayGroups = c("Earthquakes", "Major Cities", "Tectonic Plates", 
                          "Tsunami Rings", "City Labels"),
        options = layersControlOptions(collapsed = FALSE)
      )
  })
  
  observeEvent(input$map_marker_click, {
    click <- input$map_marker_click
    if (!is.null(click)) {
      if (startsWith(click$id, "eq")) {
        eq_id <- as.numeric(sub("eq", "", click$id))
        selected_quake(earthquakes()[eq_id, ])
      } else if (startsWith(click$id, "city")) {
        city_id <- as.numeric(sub("city", "", click$id))
        current_selected <- selected_cities()
        
        if (city_id %in% current_selected) {
          selected_cities(setdiff(current_selected, city_id))
        } else {
          selected_cities(c(current_selected, city_id))
        }
      }
    }
  })
  
  colorPalette <- reactive({
    n_colors <- max(3, length(unique(earthquakes()$alert_new)))
    RColorBrewer::brewer.pal(n_colors, "Set2")
  })
  
  # Use the reactive color palette in your plots
  output$yearTrendPlot <- renderPlotly({
    yearly_counts <- earthquakes() %>%
      filter(year >= 1995, year <= 2023) %>%
      group_by(year) %>%
      summarise(count = n())
    
    lm_model <- lm(count ~ year, data = yearly_counts)
    yearly_counts$predicted <- predict(lm_model)
    
    colors <- colorPalette()
    
    plot_ly() %>%
      add_trace(data = yearly_counts, 
                x = ~year, 
                y = ~count, 
                type = 'scatter', 
                mode = 'markers+lines',
                name = 'Actual Count',
                line = list(color = colors[1])) %>%
      add_trace(data = yearly_counts,
                x = ~year,
                y = ~predicted,
                type = 'scatter',
                mode = 'lines',
                name = 'Trend Line',
                line = list(color = colors[2], dash = 'dash')) %>%
      layout(title = 'Earthquake Count by Year (1995-2023) with Trend Line',
             xaxis = list(title = 'Year'),
             yaxis = list(title = 'Number of Earthquakes'),
             showlegend = TRUE)
  })
  
  # Replace the existing continentMetrics output and related code with this fixed version:
  # Replace the existing continentMetrics output and related code with this version:
  
  # Replace the existing continentMetrics output and related code with this version:
  
  metrics_data <- reactive({
    req(input$selectedContinent)
    
    # Get data for selected continent
    continent_data <- earthquakes() %>%
      filter(continent == input$selectedContinent)
    
    # Calculate current averages
    averages <- continent_data %>%
      summarise(
        sig = mean(sig, na.rm = TRUE),
        magnitude = mean(magnitude, na.rm = TRUE),
        mmi = mean(mmi, na.rm = TRUE),
        cdi = mean(cdi, na.rm = TRUE)
      )
    
    # Calculate min/max values
    ranges <- continent_data %>%
      summarise(
        sig_min = min(sig, na.rm = TRUE),
        sig_max = max(sig, na.rm = TRUE),
        magnitude_min = min(magnitude, na.rm = TRUE),
        magnitude_max = max(magnitude, na.rm = TRUE),
        mmi_min = min(mmi, na.rm = TRUE),
        mmi_max = max(mmi, na.rm = TRUE),
        cdi_min = min(cdi, na.rm = TRUE),
        cdi_max = max(cdi, na.rm = TRUE)
      )
    
    # Normalize averages for display
    max_values <- earthquakes() %>%
      summarise(
        sig = max(sig, na.rm = TRUE),
        magnitude = max(magnitude, na.rm = TRUE),
        mmi = max(mmi, na.rm = TRUE),
        cdi = max(cdi, na.rm = TRUE)
      )
    
    data.frame(
      Metric = c("Significance", "Magnitude", "MMI", "CDI"),
      Value = c(
        (averages$sig / max_values$sig) * 100,
        (averages$magnitude / max_values$magnitude) * 100,
        (averages$mmi / max_values$mmi) * 100,
        (averages$cdi / max_values$cdi) * 100
      ),
      Original = c(averages$sig, averages$magnitude, averages$mmi, averages$cdi),
      Min = c(ranges$sig_min, ranges$magnitude_min, ranges$mmi_min, ranges$cdi_min),
      Max = c(ranges$sig_max, ranges$magnitude_max, ranges$mmi_max, ranges$cdi_max)
    )
  })
  
  output$continentMetrics <- renderPlotly({
    req(metrics_data())
    
    df <- metrics_data()
    n_metrics <- nrow(df)
    angles <- seq(0, 360, length.out = n_metrics + 1)[1:n_metrics]
    
    # Create the plot without source attribute
    p <- plot_ly() %>%
      add_trace(
        type = "barpolar",
        r = df$Value,
        theta = angles,
        width = rep(360/n_metrics * 0.8, n_metrics),
        customdata = df[, c("Original", "Min", "Max", "Metric")],
        marker = list(
          color = "rgba(232, 160, 191, 0.8)",
          line = list(color = "rgb(8,48,107)", width = 1.5)
        ),
        hovertemplate = paste(
          "%{customdata[3]}<br>",
          "Value: %{customdata[0]:.1f}<br>",
          "Min: %{customdata[1]:.1f}<br>",
          "Max: %{customdata[2]:.1f}<br>",
          "<extra></extra>"
        )
      ) %>%
      layout(
        polar = list(
          angularaxis = list(
            ticktext = df$Metric,
            tickvals = angles,
            tickmode = "array",
            direction = "clockwise",
            tickfont = list(size = 12)
          ),
          radialaxis = list(
            visible = TRUE,
            range = c(0, 100),
            ticksuffix = "%",
            showline = TRUE,
            showticklabels = TRUE,
            tickfont = list(size = 10)
          ),
          bgcolor = "white"
        ),
        showlegend = FALSE,
        title = list(
          text = paste("Earthquake Metrics for", input$selectedContinent),
          font = list(size = 16)
        ),
        paper_bgcolor = "white",
        plot_bgcolor = "white",
        margin = list(t = 50, b = 50, l = 50, r = 50)
      )
    
    # Return the plot
    p
  })
  
  # Modified observer for click events
  observeEvent(event_data("plotly_click"), {
    clicked_data <- event_data("plotly_click")
    if (!is.null(clicked_data)) {
      # Access the customdata from the clicked point
      metric_data <- metrics_data()[clicked_data$pointNumber + 1, ]
      
      # Show a modal with detailed information
      showModal(modalDialog(
        title = paste("Details for", metric_data$Metric),
        HTML(sprintf(
          "<strong>Value:</strong> %.2f<br>
                 <strong>Min:</strong> %.2f<br>
                 <strong>Max:</strong> %.2f",
          metric_data$Original,
          metric_data$Min,
          metric_data$Max
        )),
        easyClose = TRUE,
        footer = modalButton("Close")
      ))
    }
  })

  
  # Add a summary table for the selected continent
  output$continentSummary <- DT::renderDT({
    req(input$selectedContinent)
    
    earthquakes() %>%
      filter(continent == input$selectedContinent) %>%
      group_by(country) %>%
      summarise(
        Total_Earthquakes = n(),
        Avg_Magnitude = round(mean(magnitude, na.rm = TRUE), 2),
        Significance = round(mean(sig, na.rm = TRUE), 2),
        Tsunami_Count = sum(tsunami == 1, na.rm = TRUE),
        CDI = round(mean(cdi, na.rm = TRUE), 2),
        MMI = round(mean(mmi, na.rm = TRUE), 2)
      ) %>%
      arrange(desc(Total_Earthquakes)) %>%
      head(10) %>%
      DT::datatable(
        options = list(
          pageLength = 10,
          dom = 'tip',  # Show table, info and pagination only
          scrollX = TRUE
        ),
        rownames = FALSE
      )
  })
  
  
  # Country comparison bar chart
  output$countryComparisonPlot <- renderPlotly({
    top_countries <- earthquakes() %>%
      filter(continent %in% c(input$continent1, input$continent2)) %>%
      group_by(continent, country) %>%
      summarise(count = n(), .groups = 'drop') %>%
      group_by(continent) %>%
      slice_max(order_by = count, n = 3)
    
    plot_ly() %>%
      add_trace(data = top_countries,
                x = ~country,
                y = ~count,
                color = ~continent,
                type = 'bar',
                text = ~count,
                textposition = 'auto') %>%
      layout(title = 'Top 3 Countries with Most Earthquakes by Continent',
             xaxis = list(title = 'Country'),
             yaxis = list(title = 'Number of Earthquakes'),
             barmode = 'group',
             showlegend = TRUE)
  })
  
  earthquakes2 <- reactive({
    # Load the data and preprocess it
    eq_data <- read.csv("Data/modified_earthquake_data.csv") %>%
      mutate(
        alert_new = tolower(alert_new),  # Make sure the alert levels are in lowercase for comparison
        magnitude = as.numeric(magnitude)  # Ensure magnitude is numeric
      ) %>%
      filter(
        !is.na(magnitude),  # Remove any rows with missing magnitudes
        magnitude >= input$magnitude[1],  # Filter by the selected magnitude range
        magnitude <= input$magnitude[2],  # Filter by the selected magnitude range
        alert_new %in% tolower(input$alert_levels)  # Filter by selected alert levels (converted to lowercase)
      )
    
    # Define marker colors based on alert levels
    eq_data$marker_color <- case_when(
      eq_data$alert_new == "yellow" ~ "#FFd800",  # Set yellow for 'Yellow' alert
      eq_data$alert_new == "orange" ~ "#FF6600",  # Set orange for 'Orange' alert
      eq_data$alert_new == "red" ~ "#BF0d0d",     # Set red for 'Red' alert
      TRUE ~ "#000000"                           # Default to black if no match
    )
    
    # Return the filtered and processed earthquake data
    return(eq_data)
  })
  
  output$tsunamiMagnitudeBox <- renderPlotly({
    # Access the filtered data from the reactive expression
    data <- earthquakes()  # Call the reactive expression to get the filtered data
    
    # Now use the filtered data in the plot
    plot_ly() %>%
      add_boxplot(
        data = data %>% filter(tsunami == 0),
        y = ~magnitude,
        name = "No Tsunami",
        boxpoints = "outliers"
      ) %>%
      add_boxplot(
        data = data %>% filter(tsunami == 1),
        y = ~magnitude,
        name = "Tsunami",
        boxpoints = "outliers"
      ) %>%
      layout(
        title = "Magnitude Distribution by Tsunami Occurrence",
        yaxis = list(title = "Magnitude"),
        showlegend = TRUE
      )
  })
  
  output$tsunamiDepthBox <- renderPlotly({
    # Access the filtered data from the reactive expression
    data <- earthquakes()  # Call the reactive expression to get the filtered data
    
    # Now use the filtered data in the plot
    plot_ly() %>%
      add_boxplot(
        data = data %>% filter(tsunami == 0),
        y = ~depth,
        name = "No Tsunami",
        boxpoints = "outliers"
      ) %>%
      add_boxplot(
        data = data %>% filter(tsunami == 1),
        y = ~depth,
        name = "Tsunami",
        boxpoints = "outliers"
      ) %>%
      layout(
        title = "Depth Distribution by Tsunami Occurrence",
        yaxis = list(title = "Depth (km)"),
        showlegend = TRUE
      )
  })
  
}


# Run the app
shinyApp(ui = ui, server = server)
