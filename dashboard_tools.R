library(tidyverse)
library(ggplot2)

get_countries<-function(df){
  df<-df%>%select(country)%>% unique()
  return (df)
}



food_consumption %>%group_by(food_category)


df<-food_consumption%>%filter(country=="Argentina") 

ggplot(df,aes(food_category,consumption)) +
  geom_bar(stat="identity")


x<-food_consumption%>%select(country)


unique(x)



get_countries(food_consumption)
