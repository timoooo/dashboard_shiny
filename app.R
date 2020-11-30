library(shiny)
library(shinydashboard)
library(tidyverse)
library(ggthemes)

get_countries <- function(df) {
  df <- df %>% select(country) %>% unique() %>% arrange(country)
  return (df)
}

get_food_cat <- function(df) {
  df <- df %>% select(food_category) %>% unique() %>% arrange(country)
  return (df)
}

df <-
  readr::read_csv(
    'https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-18/food_consumption.csv'
  )
df <-
  df %>% mutate(cons_to_emiss_ratio = consumption / co2_emmission)

ui <- dashboardPage(
  dashboardHeader(title = "Food consumption dashboard"),
  dashboardSidebar(sidebarMenu(
    menuItem(
      "Plot consumption",
      tabName = "plothome",
      icon = icon("dashboard")
    ),
    menuItem("Plot by nation",
             tabName = "plotbynation",
             icon = icon("th"))
  )),
  dashboardBody(tabItems(
    # First tab content
    tabItem(
      tabName = "plotbynation",
      h2("Consumption of food by Country"),
      
      fluidRow(
        box(plotOutput("plot1", height = 500)),
        
        box(
          title = "Select a country",
          selectInput(
            "country",
            "Country:",
            choices = get_countries(df),
            selected = "Austria"
          )
        ),
        box(
          title = "Select consumption or co2 emissions",
          selectInput(
            "type",
            "Consumptions or Emissions",
            choices = c("co2 Emmission" = "co2_emmission",
                        "consumption" = "consumption"),
            selected = "consumption"
          )
        )
      )
    ),
    tabItem(
      tabName = "plothome",
      h2("Consumption of food all countries"),
      fluidRow(box(plotOutput("homeplot")))
    )
  ))
)
server <- function(input, output) {
  output$plot1 <- renderPlot({
    df <-
      food_consumption %>% filter(country == input$country) %>% arrange(!!as.name(input$type))
    ggplot(df, aes(food_category,!!as.name(input$type), fill = food_category)) +
      geom_bar(stat = "identity") +
      theme(
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.title = element_blank()
      )
  })
  
  output$homeplot <- renderPlot({
    df %>%   ggplot(aes(x = fct_reorder(food_category, consumption), y = consumption, color = country)) +
      geom_jitter() +
      theme(legend.position = "none") +
      coord_flip()
    
  })
  
  
}

shinyApp(ui, server)
