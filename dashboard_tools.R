library(tidyverse)
library(ggplot2)

get_countries<-function(df){
  df<-df%>%select(country)%>% unique()
  return (df)
}



food_consumption %>%group_by(food_category)


df<-food_consumption%>%filter(country=="Argentina") 

ggplot(df,aes(food_category,consumption,fill=food_category)) +
  geom_bar(stat="identity") +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

df <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-18/food_consumption.csv')

df<-food_consumption%>%filter(food_category=="Beef") %>% arrange(desc(consumption))

ggplot(df,aes(country,consumption,fill=country)) +
  geom_point() +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())
