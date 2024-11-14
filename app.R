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
library(networkD3)
library(readr)

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
                 leafletOutput("map", height = "800px")),
        
        # Create the Overview tab
        tabPanel(
          title = "Overview",
          
          # Header
          div(style = "text-align: center; margin-top: 20px;", 
              h2("Overview of Earthquake Alerts and Tsunami Status")
          ),
          
          # Sankey diagram and text output
          div(style = "text-align: center; margin-top: 20px;",
              fluidRow(
                column(12, sankeyNetworkOutput("sankey_plot", height = "500px")),  # Sankey plot column adjusted
                column(12, uiOutput("explanation_text"))  # Text output column
              )
          ),
          
          # Footer
          div(style = "text-align: center; margin-top: 20px; font-size: 12px; color: grey;",
              "Data Source: Earthquake Analysis Project | Last Updated: 2024"
          )
        ),
        
        tabPanel("Time",
                 div(style = "text-align: center; margin-top: 20px;", 
                     h3()
                 ),
                 plotlyOutput("yearTrendPlot", height = "600px")
        ),
        
        tabPanel("Country Comparison",
                 div(style = "text-align: center; margin-top: 20px;", 
                     h3()
                 ),
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
        
        tabPanel(
          title = "Data View",
          div(style = "text-align: center; margin-top: 20px;", 
              h2("Top Countries by Continent Average Earthquake Data")
          ),
          fluidRow(
            column(6,
                   selectInput("continent1", 
                               "Select Continent:",
                               choices = c("North America", "South America", "Europe", 
                                           "Asia", "Africa", "Oceania"))
            ),
            dataTableOutput("continentSummary")
          )
          
        ),
       
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
      
      div(id = "notification-area", class = "notification-area")
    )
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
  
  earthquake_data<-read.csv("Data/modified_earthquake_data.csv")
  
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
        title = "Earthquake Metrics:"
      )
      mtext(paste("Name:", quake$title), side = 3, line = 0.5, cex = 0.9)
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
    
    that <- c("blue", "grey")
    names(that) <- c("Tsunami", "City")
    
    extra <- paste(
      "<span style='background-color:", that, "; width: 10px; height: 10px; display: inline-block;'></span>",
      toupper(names(that)),
      "<br>"
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
      addControl(html = paste(
        "<div style='background-color: white; padding: 10px; border-radius: 5px;'>",
        paste(extra, collapse = ""),
        "</div>"
      ),
      position = "bottomleft"
      )%>%
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
  
  my_data2 <-read.csv("Data/modified_earthquake_data.csv")
  
  my_data2 <- my_data2 %>%
    mutate(
      magnitude_bins = case_when(
        magnitude >= 6 & magnitude < 7 ~ "Magnitude 6-7",
        magnitude >= 7 & magnitude < 8 ~ "Magnitude 7-8",
        magnitude >= 8 & magnitude < 9 ~ "Magnitude 8-9",
        magnitude >= 9 ~ "Magnitude 9+",
        TRUE ~ "Unknown"
      ),
      tsunami_status = ifelse(tsunami == 1, "Tsunami", "No Tsunami")
    )
  
  # Count transitions for Sankey diagram
  link_data1 <- my_data2 %>%
    count(magnitude_bins) %>%
    rename(value = n) %>%
    mutate(source = 0, 
           target = case_when(
             magnitude_bins == "Magnitude 6-7" ~ 1,
             magnitude_bins == "Magnitude 7-8" ~ 2,
             magnitude_bins == "Magnitude 8-9" ~ 3,
             magnitude_bins == "Magnitude 9+" ~ 4
           ))
  
  link_data2 <- my_data2 %>%
    count(magnitude_bins, alert_new) %>%
    rename(value = n) %>%
    mutate(
      source = case_when(
        magnitude_bins == "Magnitude 6-7" ~ 1,
        magnitude_bins == "Magnitude 7-8" ~ 2,
        magnitude_bins == "Magnitude 8-9" ~ 3,
        magnitude_bins == "Magnitude 9+" ~ 4
      ),
      target = case_when(
        alert_new == "Yellow" ~ 5,
        alert_new == "Orange" ~ 6,
        alert_new == "Red" ~ 7
      )
    )
  
  link_data3 <- my_data2 %>%
    count(alert_new, tsunami_status) %>%
    rename(value = n) %>%
    mutate(
      source = case_when(
        alert_new == "Yellow" ~ 5,
        alert_new == "Orange" ~ 6,
        alert_new == "Red" ~ 7
      ),
      target = case_when(
        tsunami_status == "No Tsunami" ~ 8,
        tsunami_status == "Tsunami" ~ 9
      )
    )
  
  # Combine links for Sankey diagram
  links <- bind_rows(link_data1, link_data2, link_data3) %>% 
    select(source, target, value) %>% 
    na.omit()
  
  # Create nodes for Sankey plot
  nodes <- data.frame(
    name = c("Earthquakes", "Magnitude 6-7", "Magnitude 7-8", "Magnitude 8-9", "Magnitude 9+",
             "Yellow", "Orange", "Red", "No Tsunami", "Tsunami"),
    stringsAsFactors = FALSE
  )
  
  # Corrected color mapping - magnitude bins from light to dark blue
  my_color <- JS('d3.scaleOrdinal()
  .domain(["Earthquakes", "Magnitude 6-7", "Magnitude 7-8", "Magnitude 8-9", "Magnitude 9+",
           "Yellow", "Orange", "Red", "No Tsunami", "Tsunami"])
  .range(["#666666", "#E6F3FF", "#85B8FF", "#1A75FF", "#004DBF", 
          "#FFDE21", "#FFA500", "#FF0000", "#32CD32", "#4B0082"])')
  
  
  output$sankey_plot <- renderSankeyNetwork({
    sankeyNetwork(
      Links = links, 
      Nodes = nodes,
      Source = "source", 
      Target = "target", 
      Value = "value", 
      NodeID = "name", 
      colourScale = JS(my_color), 
      fontSize = 12, 
      nodeWidth = 30, 
      height = 500, 
      width = 800
    )
  })
  
    
    output$explanation_text <- renderUI({
      HTML("
    <div style='padding: 20px; line-height: 1.6;'>
      <h3>Metric Definitions</h3>
      
      <p><strong>1. MMI (Modified Mercalli Intensity)</strong><br>
      A scale used to measure the intensity of an earthquake's effect at different locations, 
      ranging from I (not felt) to XII (total destruction).</p>
      
      <p><strong>2. CDI (Community Internet Intensity Map)</strong><br>
      A crowdsourced tool that allows people to report earthquake shaking, 
      helping to estimate earthquake intensity at different locations.</p>
      
      <p><strong>3. Depth</strong><br>
      The depth of the earthquake's focus, measured in kilometers from the Earth's surface 
      to the epicenter. Affects the intensity of shaking experienced.</p>
      
      <p><strong>4. Magnitude</strong><br>
      A numerical measure of the energy released by an earthquake, typically measured 
      on the Richter scale or moment magnitude scale (Mw).</p>
      
      <p><strong>5. Significance</strong><br>
      Refers to the potential impact of the earthquake, often tied to magnitude and depth, 
      as well as the proximity to populated areas and infrastructure.</p>
    </div>
  ")
    })
  
  colorPalette <- reactive({
    n_colors <- max(3, length(unique(earthquakes()$alert_new)))
    RColorBrewer::brewer.pal(n_colors, "Set2")
  })
  
  output$yearTrendPlot <- renderPlotly({
    yearly_counts <- earthquakes() %>%
      filter(year >= 1995, year <= 2023) %>%
      group_by(year) %>%
      summarise(count = n())
    
    # Fit a smoothed loess model for smoothing
    loess_model <- loess(count ~ year, data = yearly_counts)
    yearly_counts$smoothed <- predict(loess_model)
    
    colors <- colorPalette()
    
    plot_ly() %>%
      # Add bar plot for actual counts
      add_bars(data = yearly_counts, 
               x = ~year, 
               y = ~count, 
               name = 'Actual Count', 
               marker = list(color = "#7592e9")) %>%
      # Add smoothed line
      add_lines(data = yearly_counts,
                x = ~year,
                y = ~smoothed,
                name = 'Smoothed Line',
                line = list(color = colors[2])) %>%
      layout(title = 'Earthquake Count by Year (1995-2023) with Smoothed Line',
             xaxis = list(title = ''),  # Remove axis label
             yaxis = list(title = ''),  # Remove axis label
             showlegend = TRUE)
  })
  
  
  
  # Country comparison bar chart
  output$countryComparisonPlot <- renderPlotly({

    top_countries <- earthquakes() %>%
      filter(continent %in% c(input$continent1, input$continent2)) %>%
      group_by(continent, country) %>%
      summarise(count = n(), .groups = 'drop') %>%
      arrange(desc(count)) %>%  
      group_by(continent) %>%
      slice_max(order_by = count, n = 3) %>%  
      ungroup() %>%
      arrange(continent, count) 
    

    plot_ly() %>%
      add_trace(data = top_countries,
                x = ~country,
                y = ~count,
                color = ~continent,
                type = 'bar',
                text = ~count,
                textposition = 'auto') %>%
      layout(title = 'Top 3 Countries with Most Earthquakes by Continent',
             xaxis = list(title = 'Country', categoryorder = 'array',
                          categoryarray = ~paste(continent, country, sep = "_")),
             yaxis = list(title = 'Number of Earthquakes'),
             barmode = 'group',
             showlegend = TRUE)
  })
  
  earthquakes2 <- reactive({
 
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
  
  
  output$continentSummary <- DT::renderDT({
    
    earthquakes() %>%
      filter(continent %in% c(input$continent1)) %>%
      group_by(continent, country) %>%
      summarise(
        Total_Earthquakes = n(),
        Magnitude = round(mean(magnitude, na.rm = TRUE), 2),
        Significance = round(mean(sig, na.rm = TRUE), 2),
        Tsunami_Count = sum(tsunami == 1, na.rm = TRUE),
        CDI = round(mean(cdi, na.rm = TRUE), 2),
        MMI = round(mean(mmi, na.rm = TRUE), 2),
        Depth=round(mean(depth, na.rm = TRUE), 2)
      ) %>%
      arrange(desc(Total_Earthquakes)) %>%
      head(100) %>%
      DT::datatable(
        options = list(
          pageLength = 10,
          dom = 'tip',  
          scrollX = TRUE
        ),
        rownames = FALSE
      )
  })
}



shinyApp(ui = ui, server = server)
