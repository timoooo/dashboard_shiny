library(shiny)
library(shinydashboard)
library(tidyverse)
library(ggthemes)
library(shinythemes)
library(RColorBrewer)
library(leaflet)
library(maps)
library(mapdata)
library(ggplot2)
library(plotly)
library(devtools)

get_countries <- function(df) {
  df <- df %>% select(country) %>% unique() %>% arrange(country)
  return (df)
}

get_food_cat <- function(df) {
  df <- df %>% select(food_category) %>% unique() %>% arrange(country)
  return (df)
}

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
      lng = replace(lng, country == "Taiwan", -95.712891),
      iso = replace(iso, country == "Taiwan", "TW"),
      lat = as.numeric(lat),
      lng = as.numeric(lng)
    ) %>%
    as_tibble()
  return(new_df)
}

get_full_dataset <- function() {
  df <-  readr::read_csv(
    'https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-18/food_consumption.csv'
  )
  
  countries <-
    readr::read_delim("/home/tmo/Projects/dashboard_shiny/countries.csv",
                      "\t") %>% rename(
                        iso = `country `,
                        lat = `latitude `,
                        lng = `longitude `,
                        country =
                          name
                      )
  
  return(enrich_data_with_countries(df, countries))
}


generate_leaflet_map <- function() {
  enrich_leaflet_map_with_markers <- function(full_df, leafletobj) {
    greenLeafIcon <- makeIcon(
      iconUrl = "http://leafletjs.com/examples/custom-icons/leaf-green.png",
      iconWidth = 38,
      iconHeight = 95,
      iconAnchorX = 22,
      iconAnchorY = 94,
      shadowUrl = "http://leafletjs.com/examples/custom-icons/leaf-shadow.png",
      shadowWidth = 50,
      shadowHeight = 64,
      shadowAnchorX = 4,
      shadowAnchorY = 62
    )
    
    country_data <- get_compromised_dataset(get_full_dataset())
    m <- leafletobj
    index = 1
    for (country in country_data$country) {
      m <-
        addMarkers(
          m,
          lng = country_data$lng[index],
          lat = country_data$lat[index],
          popup = generate_popup_content_view(full_df, country),
          icon = greenLeafIcon
        )
      index = index + 1
    }
    
    return(m)
  }
  
  get_compromised_dataset <- function(df) {
    country_and_coordinates <-
      df %>% select(c(country, lat, lng)) %>% unique()
    return(country_and_coordinates)
  }
  
  
  get_country_df <- function(full_df, my_country) {
    countrydf <-
      full_df %>% filter(country == my_country) %>% select(-1) %>% arrange(food_category)
    return(countrydf)
  }
  # emission in [Kg CO2/person/year]
  # consumption in [kg/person/year]
  generate_popup_content_view <- function(full_df, country) {
    countrydf <- get_country_df(full_df, country)
    content <- paste0(
      "<b>",
      country,
      "</b><br/>",
      "<table>
<thead>
  <tr>
    <th>Foodtype</th>
    <th>Consumption </th>
    <th>CO2 emissions</th>
  </tr>
</thead>
<tbody>
  <tr>
    <td>",
      as.character(countrydf$food_category[1]),
      "</td>
    <td>",
      as.numeric(countrydf$consumption[1]),
      "</td>
    <td>",
      as.numeric(countrydf$co2_emmission[1]),
      "</td>
  </tr>
  <tr>
    <td>",
      as.character(countrydf$food_category[2]),
      "</td>
    <td>",
      as.numeric(countrydf$consumption[2]),
      "</td>
    <td>",
      as.numeric(countrydf$co2_emmission[2]),
      "</td>
  </tr>
  <tr>
    <td>",
      as.character(countrydf$food_category[3]),
      "</td>
    <td>",
      as.numeric(countrydf$consumption[3]),
      "</td>
    <td>",
      as.numeric(countrydf$co2_emmission[3]),
      "</td>
  </tr>
  <tr>
    <td>",
      as.character(countrydf$food_category[4]),
      "</td>
    <td>",
      as.numeric(countrydf$consumption[4]),
      "</td>
    <td>",
      as.numeric(countrydf$co2_emmission[4]),
      "</td>
  </tr>
  <tr>
    <td>",
      as.character(countrydf$food_category[5]),
      "</td>
    <td>",
      as.numeric(countrydf$consumption[5]),
      "</td>
    <td>",
      as.numeric(countrydf$co2_emmission[5]),
      "</td>
  </tr>
  <tr>
    <td>",
      as.character(countrydf$food_category[6]),
      "</td>
    <td>",
      as.numeric(countrydf$consumption[6]),
      "</td>
    <td>",
      as.numeric(countrydf$co2_emmission[6]),
      "</td>
  </tr>
  <tr>
    <td>",
      as.character(countrydf$food_category[7]),
      "</td>
    <td>",
      as.numeric(countrydf$consumption[7]),
      "</td>
    <td>",
      as.numeric(countrydf$co2_emmission[7]),
      "</td>
  </tr>
  <tr>
    <td>",
      as.character(countrydf$food_category[8]),
      "</td>
    <td>",
      as.numeric(countrydf$consumption[8]),
      "</td>
    <td>",
      as.numeric(countrydf$co2_emmission[8]),
      "</td>
  </tr>
  <tr>
    <td>",
      as.character(countrydf$food_category[9]),
      "</td>
    <td>",
      as.numeric(countrydf$consumption[9]),
      "</td>
    <td>",
      as.numeric(countrydf$co2_emmission[9]),
      "</td>
  </tr>
  <tr>
    <td>",
      as.character(countrydf$food_category[10]),
      "</td>
    <td>",
      as.numeric(countrydf$consumption[10]),
      "</td>
    <td>",
      as.numeric(countrydf$co2_emmission[10]),
      "</td>
  </tr>
  <tr>
    <td>",
      as.character(countrydf$food_category[11]),
      "</td>
    <td>",
      as.numeric(countrydf$consumption[11]),
      "</td>
    <td>",
      as.numeric(countrydf$co2_emmission[11]),
      "</td>
  </tr>
</tbody>
</table>"
    )
    
    return(content)
    
  }
  
  
  
  
  full_df <- get_full_dataset()
  data_regions <- full_df %>% get_countries()
  mapStates <- maps::map("world",
                         regions = data_regions$country,
                         fill = T)
  
  
  my_map <-
    leaflet(data = mapStates, options = leafletOptions(minZoom = 2, maxZoom = 4)) %>%
    addTiles() %>%
    addPolygons(fillColor = topo.colors(10, alpha = NULL), stroke = FALSE)
  
  my_map <- enrich_leaflet_map_with_markers(full_df, my_map)
  
  
  return(my_map)
}


get_top_10_by_consumption <- function() {
  df <- get_full_dataset()
  ret_df <-
    df %>% group_by(country) %>% summarise(sum_cons = sum(consumption),
                                           sum_co2 = sum(co2_emmission)) %>% top_n(10, sum_cons) %>% arrange(desc(sum_cons))
  return(ret_df)
}

get_top_10_by_co2 <- function() {
  df <- get_full_dataset()
  ret_df <-
    df %>% group_by(country) %>% summarise(sum_cons = sum(consumption),
                                           sum_co2 = sum(co2_emmission)) %>% top_n(10, sum_co2) %>% arrange(desc(sum_co2))
  return(ret_df)
}

get_top_10_by_by_efficiency <- function() {
  df <- get_full_dataset()
  ret_df <-
    df %>% group_by(country) %>% summarise(sum_cons = sum(consumption),
                                           sum_co2 = sum(co2_emmission)) %>%
    mutate(efficiency = sum_cons / sum_co2) %>% top_n(10, efficiency) %>% arrange(desc(efficiency))
  return(ret_df)
}


get_top_10_by_plot_consm <- function() {
  plot <-
    get_top_10_by_consumption() %>% ggplot(aes(x = reorder(country,-sum_cons), sum_cons, fill = country)) +
    geom_bar(stat = "identity", width = 0.8) +
    theme(
      axis.title.x = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      legend.title = element_blank(),
      
    ) + ylab("Summary of consumption") +
    geom_text(aes(
      x = country,
      y = sum_cons + 7,
      label = round(sum_cons, 1)
    )) +
    theme_void()
  
  return(plot)
}

get_mean_df <- function() {
  df <- get_full_dataset()
  ret_df <-
    df %>% mutate(
      consumption = replace(consumption, consumption == 0, NA),
      co2_emmission = replace(co2_emmission, co2_emmission ==
                                0, NA)
    ) %>%
    group_by(food_category) %>%
    summarise(
      consumption = mean(consumption, na.rm = T),
      co2_emmission = mean(co2_emmission, na.rm = T)
    )
  return(ret_df)
}

get_mean_df()

get_top_10_by_plot_efficiency <- function() {
  plot <-
    get_top_10_by_by_efficiency() %>% ggplot(aes(
      y = efficiency,
      x = reorder(country,-efficiency),
      fill = country
    )) +
    geom_bar(stat = "identity", width = 0.8) +
    theme(
      axis.title.x = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      legend.title = element_text("Most efficient countries"),
      
    ) + ylab("Efficiency (consumption/emmissions)") +
    geom_text(aes(
      x = country,
      y = efficiency + 0.05,
      label = round(efficiency, 1)
    )) +
    xlab("Countries") +
    theme_void()
  
  return(plot)
}

get_top_10_by_plot_co2 <- function() {
  plot <-
    get_top_10_by_co2() %>% ggplot(aes(x = reorder(country,-sum_co2), sum_co2, fill = country)) +
    geom_bar(stat = "identity", width = 0.8) +
    theme(
      axis.title.x = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      legend.title = element_blank(),
      
    ) + ylab("Summary of co2 emmission") +
    geom_text(aes(
      x = country,
      y = sum_co2 + 12,
      label = round(sum_co2, 1)
    )) +
    theme_void()
  
  return(plot)
}


# emission in [Kg CO2/person/year]
# consumption in [kg/person/year]


get_plot_for_country_pie_cons <- function(arg_country) {
  df <- get_full_dataset()
  ret_df <-
    df %>% filter(country == !!arg_country) %>% select(food_category, co2_emmission, consumption) %>% arrange(desc(consumption)) %>%
    mutate(food_category = factor(food_category))
  
  p <-
    ret_df %>% ggplot(aes(x = "", y = consumption, fill = food_category)) +
    geom_bar(width = 1,
             stat = "identity",
             color = "black") +
    theme(
      axis.line = element_blank(),
      plot.title = element_text(hjust = 0.5),
      axis.ticks.y = element_blank(),
      axis.ticks.x = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank()
    ) +
    labs(
      fill = "food category",
      x = NULL,
      y = NULL,
      caption = "Consumption of a country in [kg/person/year]"
    ) +
    coord_polar(theta = "y", start = 0) +
    theme_void()
  return(p)
}



get_plot_for_country_bar_cons <- function(arg_country) {
  df <- get_full_dataset()
  ret_df <-
    df %>% filter(country == !!arg_country) %>% select(food_category, co2_emmission, consumption) %>% arrange(desc(consumption)) %>%
    mutate(
      food_category = factor(food_category),
      cat = factor(!!arg_country)) %>% arrange(food_category)
  
  mean_df <- get_mean_df() %>% mutate(cat = as.factor("mean"))
  full_df <- bind_rows(ret_df,mean_df)
  p <-
    full_df %>% ggplot() +
    geom_bar(aes(
      y = consumption,
      x = reorder(food_category, consumption),
      fill = food_category,
      group=interaction(food_category,cat),fill=food_category,alpha=cat
    ),
      stat = "identity",
      color = "black",
      position = "dodge2",
    width=1
    )   + geom_text(aes(
      x = food_category,
      y = consumption + 7,
      label = round(consumption, 2)
    ),
    position = position_dodge2(width=1,preserve = "single")) + 
    theme(
      axis.line = element_blank(),
      plot.title = element_text(hjust = 0.5),
      axis.ticks.y = element_blank(),
      axis.ticks.x = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      legend.title = element_blank()
    ) +
    theme_void() +
    scale_fill_discrete(name = "") +
    labs(caption = "Consumption of a country in [kg/person/year]") +
    theme_void() +
    scale_alpha_manual(values = c(1, .4))
  
  return(p)
}

get_plot_for_country_bar_cons("Austria")


get_plot_for_country_pie_co2 <- function(arg_country) {
  df <- get_full_dataset()
  ret_df <-
    df %>% filter(country == !!arg_country) %>% select(food_category, co2_emmission, consumption) %>% arrange(desc(consumption)) %>%
    mutate(food_category = factor(food_category))
  
  p <-
    ret_df %>% ggplot(aes(x = "", y = co2_emmission, fill = food_category)) +
    geom_bar(width = 1,
             stat = "identity",
             color = "black") +
    theme(
      axis.line = element_blank(),
      plot.title = element_text(hjust = 0.5),
      axis.ticks.y = element_blank(),
      axis.ticks.x = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank()
    ) +
    labs(
      fill = "food category",
      x = NULL,
      y = NULL,
      caption = "CO2 emmissions of a country in [Kg CO2/person/year]"
    ) +
    coord_polar(theta = "y", start = 0) +
    theme_void()
  return(p)
}

get_plot_for_country_bar_co2 <- function(arg_country) {
  df <- get_full_dataset()
  ret_df <-
    df %>% filter(country == !!arg_country) %>% select(food_category, co2_emmission, consumption) %>% arrange(desc(consumption)) %>%
    mutate(
      food_category = factor(food_category),
      cat = factor(!!arg_country)) %>% arrange(food_category)
  
  mean_df <- get_mean_df() %>% mutate(cat = as.factor("mean"))
  full_df <- bind_rows(ret_df,mean_df)
  
  p <-
    full_df %>% ggplot() +
    geom_bar(aes(
      y = co2_emmission,
      x = reorder(food_category, co2_emmission),
      fill = food_category,
      group=interaction(food_category,cat),alpha=cat
    ),
    stat = "identity",
    color = "black",
    position = "dodge2",
    width=1) +
    geom_text(aes(
      x = food_category,
      y = co2_emmission + 7,
      label = round(co2_emmission, 2)),
      position = position_dodge2(width=1,preserve = "single")
      ) +
    theme(
      axis.line = element_blank(),
      plot.title = element_text(hjust = 0.5),
      axis.ticks.y = element_blank(),
      axis.ticks.x = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      legend.title = element_blank()
    ) +
    theme_void() +
    scale_fill_discrete(name = "") +
    labs(caption = "CO2 emmissions of a country in [Kg CO2/person/year]")+
    scale_alpha_manual(values = c(1, .4))
  
  return(p)
}



ui <- dashboardPage(
  skin = "green",
  dashboardHeader(title = "Food consumption dashboard"),
  dashboardSidebar(sidebarMenu(
    menuItem(
      "Interactive Worldmap",
      tabName = "Worldmap",
      icon = icon("map-signs")
    ),
    menuItem(
      "Top 10 countrys",
      tabName = "top10",
      icon = icon("not-equal")
    ),
    menuItem(
      "Country specific data",
      tabName = "countrydata",
      icon = icon("flag")
    )
  )),
  dashboardBody(tabItems(
    tabItem(
      tabName = "Worldmap",
      h2("An interactive map to show data of all countries"),
      h4("Consumption measured in [kg/person/year]."),
      h4("CO2 emmission in [kg CO2/person/year]."),
      leafletOutput("leaflet", height = 1200)
    ),
    #plot top 10 by cons relation & co2_emmission & efficiency
    tabItem(
      tabName = "top10",
      h2("Top 10 countrys by consumption, co2 emmissions or efficiency"),
      
      fluidRow(
        box(
          width = 2,
          title = "Select a chart type",
          radioButtons(
            "radio",
            h4("Chart types:"),
            choices = list(
              "Consumption" = 1,
              "CO2 emmissions" = 2,
              "Efficiency" = 3
            ),
            selected = 3
          )
        ),
        box(width = 10, plotOutput("plottop10", height = 1000))
      )
    ),
    tabItem(
      tabName = "countrydata",
      h2("Select a country to inspect"),
      fluidRow(
        box(
          width = 2,
          skin = "green",
          selectInput(
            "country",
            "Select a country:",
            choices = get_full_dataset() %>% get_countries(),
            selected = "Austria"
          )
        ),
        box(width = 10, plotOutput("country", height = 700)),
        
      ),
      fluidRow(box(
        width = 2,
        radioButtons(
          "radio1",
          h4("Chart types:"),
          choices = list("Consumption" = 1,
                         "CO2 emmissions" = 2),
          selected = 2
        )
      )
      , box(width = 10,
            plotOutput("country1")))
    )
  ))
)

server <- function(input, output) {
  output$leaflet <- renderLeaflet(generate_leaflet_map())
  
  output$plottop10 <- renderPlot({
    if (input$radio == 1) {
      return(get_top_10_by_plot_consm())
    } else if (input$radio == 2) {
      return(get_top_10_by_plot_co2())
      
    } else{
      return(get_top_10_by_plot_efficiency())
    }
  })
  
  output$country <- renderPlot({
    if (input$radio1 == 1) {
      return(get_plot_for_country_pie_cons(input$country))
    } else {
      return(get_plot_for_country_pie_co2(input$country))
    }
  })
  
  output$country1 <- renderPlot({
    if (input$radio1 == 1) {
      return(get_plot_for_country_bar_cons(input$country))
    } else {
      return(get_plot_for_country_bar_co2(input$country))
    }
  })
  
}

shinyApp(ui, server)
