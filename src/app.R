library(tidyverse)
library(dplyr)
library(tidyr)
library(shiny)
library(ggplot2)
library(bslib)
library(leaflet)

## read data
df <- read.csv("data/listings_jan25.csv")

## data cleaning 
df <- df |> 
  select(neighbourhood_cleansed, property_type, price, minimum_nights, latitude, longitude, name, accommodates, beds, bathrooms_text) |> 
  filter(price != "") |> 
  mutate(price = as.integer(gsub("[$]", "", price))) |> 
  drop_na(price)


## UI 

# Layout
ui <- fluidPage(
  
  theme = bs_theme(bg = "#f9f9f9", fg = "#333333", primary = "#007bff"),
  
  tags$style(HTML("
    body {
      font-family: Arial, sans-serif;
      background-color: #ffffff;  
      padding: 20px;
    }
    .container {
      background-color: #ffffff;
      padding: 20px;
      border-radius: none;
      box-shadow: 0 4px 8px rgba(0,0,0,0.1);
      margin-bottom: 20px;
    }
    .title {
      font-size: 24px;
      font-weight: bold;
      margin-bottom: 20px;
      color: #FF5A5F;  
    }
    .selection-container {
      padding: 5px 15px;
      border: 3px solid #888;
      border-radius: 10px;
      background-color: #ffffff;
      margin-bottom: 12 px;
      font-size: 15px;
    }
    .plot-container {
      padding: 10px;
      border: 1px solid #ddd;
      border-radius: 8px;
      background-color: #fafafa;
      margin-bottom: 20px;
    }
    .summary-container {
      padding: 18px;
      border: 3px solid #888;
      border-radius: 8px;
      background-color: #ffffff;
      margin-bottom: 20px;
      text-align: center;
      font-size: 15px
    }.
    strong {
      display: block;
      margin-top: 10px;
      color: #333;
    }.summary-container .shiny-text-output {
  font-size: 24px; 
  font-weight: bold;
  color: #333;
}
  ")),
  
  
  div(class = "container",
      div(class = "title", "Airbnb Listings - City of Vancouver"),
      fluidRow(
        column(4,
       div(class='selection-container',
          selectInput(
            'neighbourhood',
            strong('Select Neighbourhood'),
            choices = sort(unique(df$neighbourhood_cleansed)),
            selected = "Downtown"
          ),
        ),
        ),

        column(4,
        div(class = "summary-container",
          textOutput("output_average_price"),
          strong("Average Price Per Night")
          
          )
            ),
            column(4,
            div(class = "summary-container",
              textOutput("output_num_listings"),
              strong("Number Of Listings In the Neighbourhood")
              
              )
            ),
          ),
      
      fluidRow(
        column(4,
          div(class = "plot-container",
              plotOutput("property_type_plot", height = "500px")
          )
        ),
        column(8,
               div(class = "map-container",
                   leafletOutput("map_plot", height = "500px")
               )
        )
      )
  )
)

# callbacks
server <- function(input, output, session) {
  
  ## property_type_callback 
  output$property_type_plot <- renderPlot({
    
    filtered_df <- df |> 
      filter(neighbourhood_cleansed == input$neighbourhood) |> 
      add_count(property_type)
    
    return(
      ggplot(filtered_df, aes(y = reorder(property_type, n))) +
        geom_bar(stat = 'count', fill = "#FF5A5F", color = "white") +
        labs(y = "Property type", x = "Count") +
        theme_minimal(base_size = 14) + 
        theme(
          axis.title.x = element_text(size=12, margin = margin(r = 20)), 
          axis.text.x = element_text(size = 10),
          axis.title.y = element_text(size=12, margin = margin(r = 30)),  
          axis.text.y = element_text(size = 10)
        )
    ) 
  })
  
  ## average price callback 
  output$output_average_price <- renderText({
    
    filtered_df <- df |> 
      filter(neighbourhood_cleansed == input$neighbourhood)
    
    return(
      paste0("$", round(mean(filtered_df$price), 0))
    )
  })
  
  ## number of listings callback 
  output$output_num_listings <- renderText({
    
    filtered_df <- df |> 
      filter(neighbourhood_cleansed == input$neighbourhood)
    
    return(
      nrow(filtered_df)
    )
  })
  

  ## Map callback
  output$map_plot <- renderLeaflet({
    
    filtered_df <- df |> 
      filter(neighbourhood_cleansed == input$neighbourhood)
    
    leaflet(data = filtered_df) |> 
      # Use OpenStreetMap explicitly
      addProviderTiles(providers$OpenStreetMap) |> 
      addCircleMarkers(
        lng = ~longitude, lat = ~latitude,
        radius = 10,
        color = "#FF5A5F",
        stroke = FALSE,
        fillOpacity = 0.7,
        label = ~paste0(
          "Price: $", price,"; ",
          "Bedrooms: ", beds, "; ",
          "Accommodates: ", accommodates
        ),
        clusterOptions = markerClusterOptions(
          maxClusterRadius = 70
        )

      ) |> 
      setView(
        lng = mean(filtered_df$longitude, na.rm = TRUE),
        lat = mean(filtered_df$latitude, na.rm = TRUE),
        zoom = 14
      )
  })
  
}

# Run the app/dashboard
shinyApp(ui, server)



