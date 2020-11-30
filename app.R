library(shiny)
library(shinydashboard)
library(tidyverse)
library(ggthemes)

get_countries<-function(df){
  df<-df%>%select(country)%>% unique() %>% arrange(country)
  return (df)
}

df <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-18/food_consumption.csv')


ui <- dashboardPage(
  dashboardHeader(title = "Food consumption dashboard"),
  dashboardSidebar(    sidebarMenu(
    menuItem("Plot 1", tabName = "dashboard", icon = icon("dashboard")),
    menuItem("Widgets", tabName = "widgets", icon = icon("th"))
  )),
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "dashboard",
              h2("Consumption of food by Country"),
              
              fluidRow(
                box(plotOutput("plot1", height = 500)),
                
                box(
                  title = "Select a country",
                  selectInput("country", "Country:",
                              choices = get_countries(df),selected = "Austria")
                )
              )
      ),
      
      # Second tab content
      tabItem(tabName = "widgets",
              h2("Widgets tab content")
      )
    )
  )
)
server <- function(input, output) {
  set.seed(122)
  
  
    output$plot1<-renderPlot({
    df<-food_consumption%>%filter(country==input$country) 
    ggplot(df,aes(food_category,consumption,fill=food_category))+
      geom_bar(stat="identity")+
      theme(axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank(),
            legend.title=element_blank())
  })
  
  
}

shinyApp(ui, server)

