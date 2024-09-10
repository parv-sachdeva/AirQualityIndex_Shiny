library(httr)
library(maps)
library(glue)
library(rvest)
library(ggmap)
library(naniar)
library(raster)
library(leaflet)
library(tidyverse)
library(tidygeocoder)
library(bslib)
library(ggplot2)

get_province_code_by_name <- function(name, tolower=TRUE) {
    provinces <- list(
        "ONTARIO"="ON",
        "BRITISH COLUMBIA"="BC",
        "MANITOBA"="MB",
        "NEW BRUNSWICK"="NB",
        "NEWFOUNDLAND AND LABRADOR"="NL",
        "NOVA SCOTIA"="NS",
        "NORTHWEST TERRITORIES"="NT",
        "NUNAVAT"="NU",
        "PRINCE EDWARD ISLAND"="PE",
        "QUEBEC"="QC",
        "SASKATCHEWAN"="SK",
        "YUKON"="YT"
    )
    if(tolower) return(tolower(provinces[[toupper(name)]]))
    return(provinces[[toupper(name)]])
}

download_coords <- function(province, aqi_df) {
    withProgress(message = glue('Fetching coordinates for {province}'), value = 0, {
        province_code <- get_province_code_by_name(province)
        aqi_df_w_location <- aqi_df %>% mutate(Country='Canada') %>% geocode(city=City, country=Country)
        aqi_df_w_location<- aqi_df_w_location %>% select(
            City,
            latitude=lat,
            longitude=long
        )
        write.csv(aqi_df_w_location, glue("data/{province_code}_cities_coordinates.csv"), row.names=F)
    })
    return(aqi_df_w_location)
}

get_coords_by_province <- function(province, aqi_df) {
    province_code <- get_province_code_by_name(province)
    coords_file=glue("data/{province_code}_cities_coordinates.csv")
    if(file.exists(coords_file)) {
        coords <- read.csv(coords_file)
        return(coords)
    }else{
        return(download_coords(province, aqi_df))
    }
}

fetch_province_aqi <- function(province) {
    province_code <- get_province_code_by_name(province)
    url = glue("https://weather.gc.ca/airquality/pages/provincial_summary/{province_code}_e.html")
    aqi_webpage <- rvest::read_html(url)
    aqi_df <- aqi_webpage %>% 
        html_elements("table") %>% 
        .[2] %>% html_table() %>% 
        as.data.frame() %>% slice(-1) %>%
        rename(City='Location.and.sub.locations') %>%
        replace_with_na_all(~.x == "N/A") %>%
        mutate(Observed.Conditions = coalesce(Observed.Conditions, Forecast.Maximums)) %>%
        mutate(
            AQI = as.numeric(str_extract(Observed.Conditions, "\\d+")),
            Risk = str_extract(Observed.Conditions, "[A-Za-z ]+")
            # Observed.Conditions = coalesce(Observed.Conditions, Forecast.Maximums.1, Forecast.Maximums.2, Forecast.Maximums.4)
        )
    coords <- get_coords_by_province(province, aqi_df=aqi_df)
    aqi_df <- aqi_df %>%
        inner_join(
            coords,
            by=c('City'='City')
        )
}

ui <- page_sidebar(
  title = "Canadian Cities AQI Dashboard",
  sidebar = sidebar(
    title = "Canadian Cities AQI Dashboard",
    selectInput(
      "province", "Select Province",
      c("Ontario", "British Columbia", "Manitoba", "New Brunswick", "Newfoundland and Labrador", "Nova Scotia", "Northwest Territories", "Nunavat", "Prince Edward Island", "Quebec", "Saskatchewan", "Yukon")
    )
    # numericInput("bins", "Number of bins", 30)
  ),
    img(src="https://weather.gc.ca/cacheable/images/img/aqhi_scale_small_en.png", height="15%", width="40%",  align = "right"),
  card(
    card_header("Map"),
    leafletOutput("map_plot")
  )
)

server <- function(input, output) {
    aqi_data <- reactiveValues()
    observeEvent(input$province,{
        aqi_df <- fetch_province_aqi(input$province)
        aqi_data$aqi_df <- aqi_df
    })
    output$map_plot <- renderLeaflet({
        req(aqi_data$aqi_df)
        
        palette <- colorNumeric(
            palette = c("#00CCFF", "#0099CC", "#006699", "#FFFF00", "#FFCC00", "#FF9933", "#FF6666", "#FF0000", "#CC0000", "#990000", "#660000"),
            domain = seq(1,11),
            na.color = "#808080"
        )

        leaflet(aqi_data$aqi_df) %>%
            addTiles() %>%
            addCircleMarkers(
                ~longitude, ~latitude,
                color = ~palette(AQI),
                stroke = FALSE, fillOpacity = 0.8,
                radius = 8,
                popup = ~paste0(City, "<br>AQI: ", AQI, "<br>Risk: ", Risk)
            ) %>%
            addLegend(
                "topright",
                pal = palette, values = ~AQI,
                title = "AQI",
                opacity = 1
            )
    })
}

shinyApp(ui, server)