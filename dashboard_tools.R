library(tidyverse)
library(ggplot2)
#https://github.com/rfordatascience/tidytuesday/blob/master/data/2020/2020-02-18/readme.md
get_countries<-function(df){
  df<-df%>%select(country)%>% unique()
  return (df)
}



food_consumption %>%group_by(food_category)
df <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-18/food_consumption.csv')


df<-food_consumption%>%filter(country=="Argentina") 
df
ggplot(df,aes(food_category,co2_emmission,fill=food_category)) +
  geom_bar(stat="identity") +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

df <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-18/food_consumption.csv')

df<-food_consumption%>%filter(food_category=="Beef") %>% arrange(desc(consumption))
df
ggplot(df,aes(country,consumption)) +
  geom_point() +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

df <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-18/food_consumption.csv')
df <- df %>% mutate(cons_to_emiss_ratio=co2_emmission/consumption)
df

df%>%filter(country=="Austria")


df %>%   ggplot(aes(x = fct_reorder(food_category, consumption), y = consumption, color = country)) +
  geom_jitter() +
  theme(legend.position = "none") +
  coord_flip()
