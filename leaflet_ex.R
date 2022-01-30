library(leaflet)
library(maps)
library(mapdata)
library(ggplot2)
library(plotly)
library(tidyverse)

#### HELPER FUNCTIONS ####
get_countries <- function(df) {
  df <- df %>% select(country) %>% unique()
  return (df)
}

df <-  readr::read_csv(
  'https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-18/food_consumption.csv'
)

countries <-
  readr::read_delim("/home/tmo/Projects/dashboard_shiny/countries.csv", "\t") %>% rename(
    iso = `country `,
    lat = `latitude `,
    lng = `longitude `,
    country =
      name
  )

### data prep

enrich_data_with_countries <- function(df, countries) {
  new_df <- merge(df, countries, by = "country", all.x = T) %>%
    mutate(
      country = replace(country, country == "Hong Kong SAR. China", "China:Hong Kong"),
      country = replace(country, country == "United Kingdom", "UK"),
      country = replace(country, country == "Taiwan. ROC", "Taiwan")
    ) %>%
    filter(
      country != "Congo",
      country != "Trinidad and Tobago",
      country != "Macedonia",
      country != "Myanmar"
    ) %>%
    mutate(
      lat = replace(lat, country == "China:Hong Kong", 22.396428),
      lng = replace(lng, country == "China:Hong Kong", 114.109497),
      iso = replace(iso, country == "China:Hong Kong", "HK"),
      lat = replace(lat, country == "USA", 22.396428),
      lng = replace(lng, country == "USA", 114.109497),
      iso = replace(iso, country == "USA", "US"),
      lat = replace(lat, country == "Taiwan", 37.09024),
      lng = replace(lng, country == "Taiwan",-95.712891),
      iso = replace(iso, country == "Taiwan", "TW")
    ) %>%
    as_tibble()
  return(new_df)
}

new_df<-enrich_data_with_countries(df,countries)
new_df%>%get_countries() %>% View()
data_regions <- new_df%>%get_countries()

mapStates <-maps::map("world",
                       regions = data_regions$country,
                      fill=T,
                      wrap = T)

map_nopopup <- leaflet(data = mapStates, options = leafletOptions(minZoom = 2, maxZoom = 5)) %>%
  addTiles() %>%
  addPolygons(fillColor = topo.colors(10, alpha = NULL), stroke = FALSE) 
  

map_nopopup
#create for loop for popups and enrich with information
#https://rstudio.github.io/leaflet/popups.html

add_popups_to_leaflet <- function(leaf, countries) {
  leaf + addPolygons()
  
  
  
  
}


