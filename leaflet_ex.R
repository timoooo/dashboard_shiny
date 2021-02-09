library(leaflet)
library(maps)
library(mapdata)
library(ggplot2)
library(plotly)
library(tidyverse)
library(devtools)

devtools::install_github("hadley/emo")
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
      iso = replace(iso, country == "Taiwan", "TW"),
      lat = as.numeric(lat),
      lng = as.numeric(lng)
    ) %>%
    as_tibble()
  return(new_df)
}

get_full_dataset<-function(){
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
  
  return(enrich_data_with_countries(df,countries))
}

get_country_df<-function(my_country){
  
  full_df<-get_full_dataset()
  countrydf<-full_df%>%filter(country==my_country)%>%select(-1) %>% arrange(food_category)
  return(countrydf)
}
# emission in [Kg CO2/person/year]
# consumption in [kg/person/year]
generate_popup_content_view<-function(country){
  countrydf<-get_country_df(country)
  content<-paste0("<b>",country,"</b><br/>","<table>
<thead>
  <tr>
    <th>Foodtype</th>
    <th>Consumption </th>
    <th>CO2 emissions</th>
  </tr>
</thead>
<tbody>
  <tr>
    <td>", as.character(countrydf$food_category[1]),"</td>
    <td>", as.numeric(countrydf$consumption[1]),"</td>
    <td>", as.numeric(countrydf$co2_emmission[1]),"</td>
  </tr>
  <tr>
    <td>", as.character(countrydf$food_category[2]),"</td>
    <td>", as.numeric(countrydf$consumption[2]),"</td>
    <td>", as.numeric(countrydf$co2_emmission[2]),"</td>
  </tr>
  <tr>
    <td>", as.character(countrydf$food_category[3]),"</td>
    <td>", as.numeric(countrydf$consumption[3]),"</td>
    <td>", as.numeric(countrydf$co2_emmission[3]),"</td>
  </tr>
  <tr>
    <td>", as.character(countrydf$food_category[4]),"</td>
    <td>", as.numeric(countrydf$consumption[4]),"</td>
    <td>", as.numeric(countrydf$co2_emmission[4]),"</td>
  </tr>
  <tr>
    <td>", as.character(countrydf$food_category[5]),"</td>
    <td>", as.numeric(countrydf$consumption[5]),"</td>
    <td>", as.numeric(countrydf$co2_emmission[5]),"</td>
  </tr>
  <tr>
    <td>", as.character(countrydf$food_category[6]),"</td>
    <td>", as.numeric(countrydf$consumption[6]),"</td>
    <td>", as.numeric(countrydf$co2_emmission[6]),"</td>
  </tr>
  <tr>
    <td>", as.character(countrydf$food_category[7]),"</td>
    <td>", as.numeric(countrydf$consumption[7]),"</td>
    <td>", as.numeric(countrydf$co2_emmission[7]),"</td>
  </tr>
  <tr>
    <td>", as.character(countrydf$food_category[8]),"</td>
    <td>", as.numeric(countrydf$consumption[8]),"</td>
    <td>", as.numeric(countrydf$co2_emmission[8]),"</td>
  </tr>
  <tr>
    <td>", as.character(countrydf$food_category[9]),"</td>
    <td>", as.numeric(countrydf$consumption[9]),"</td>
    <td>", as.numeric(countrydf$co2_emmission[9]),"</td>
  </tr>
  <tr>
    <td>", as.character(countrydf$food_category[10]),"</td>
    <td>", as.numeric(countrydf$consumption[10]),"</td>
    <td>", as.numeric(countrydf$co2_emmission[10]),"</td>
  </tr>
  <tr>
    <td>", as.character(countrydf$food_category[11]),"</td>
    <td>", as.numeric(countrydf$consumption[11]),"</td>
    <td>", as.numeric(countrydf$co2_emmission[11]),"</td>
  </tr>
</tbody>
</table>")
  
  return(content)
  
}

get_compromised_dataset<-function(df){
  country_and_coordinates<-df%>% select(c(country,lat,lng)) %>% unique()
  return(country_and_coordinates)
}


enrich_leaflet_map_with_markers<-function(leafletobj){
  greenLeafIcon <- makeIcon(
    iconUrl = "http://leafletjs.com/examples/custom-icons/leaf-green.png",
    iconWidth = 38, iconHeight = 95,
    iconAnchorX = 22, iconAnchorY = 94,
    shadowUrl = "http://leafletjs.com/examples/custom-icons/leaf-shadow.png",
    shadowWidth = 50, shadowHeight = 64,
    shadowAnchorX = 4, shadowAnchorY = 62
  )
  
  country_data<-get_compromised_dataset(get_full_dataset())
  m<-leafletobj
  index = 1
  for(country in country_data$country){
    m<-addMarkers(m,lng = country_data$lng[index],lat=country_data$lat[index],
                    popup = generate_popup_content_view(country), icon = greenLeafIcon)
    index = index+1
  }
  
  return(m)
}


new_df<-get_full_dataset()
test<-new_df%>%filter(country=="Austria") %>%select(-1) %>% arrange(food_category)

new_df%>% select(c(country,lat,lng)) %>% distinct()
test$food_category[1]
data_regions <- new_df%>%get_countries()

mapStates <-maps::map("world",
                      regions = data_regions$country,
                      fill=T)


my_map <-leaflet(data = mapStates, options = leafletOptions(minZoom = 2, maxZoom = 5)) %>%
  addTiles() %>%
  addPolygons(fillColor = topo.colors(10, alpha = NULL), stroke = FALSE) 
#addMarkers(data = get_compromised_dataset(new_df),~lng,~lat,popup = generate_popup_content_view(country),options = popupOptions(closeButton = FALSE))

x<-enrich_leaflet_map_with_markers(my_map)
x
