library(tidyverse) 
library(readr)
library(dplyr)
library(ggplot2)
netflixDataset = read.csv(file = "C:\\Users\\Alessio\\Documents\\Università\\Data Science\\netflix_daily_top_10.csv")

netflixDataset = netflixDataset %>% 
  rename(date = "As.of",daystop10 = "Days.In.Top.10", viewerscore = "Viewership.Score", netflixexclusive = "Netflix.Exclusive",netflixreleasedate = "Netflix.Release.Date") %>%
  rename_with(tolower)

View(netflixDataset)

unique(netflixDataset$title)

#Top 10 movies
top10movies = 
  netflixDataset %>% 
  count(title) %>% 
  rename(numberofdays = n) %>% 
  arrange(-numberofdays) %>% 
  head(10)
View(top10movies)

#To find the Type count and Production count
netflixDatasetfilter = 
  netflixDataset %>% 
  select(title, netflixexclusive, type)
View(netflixDatasetfilter)

netflixDatasetfilter = 
  unique(netflixDatasetfilter) %>% 
  arrange(title)

#Type count
moviestype = 
  netflixDatasetfilter %>% 
  count(type) %>% 
  rename(count = n) %>% 
  arrange(-count)

View(moviestype)

ggplot(data=moviestype) +
  geom_col(mapping = aes(x= fct_rev(fct_reorder(type, count)), y=count, fill=count)) + 
  labs(title = "Types Count", x = "Types", y = "Count")+
  theme(legend.position = "none") +
  geom_text(aes(x= type, y = count, label = count, vjust = -0.2))

ggplot(data = top10movies) + 
  geom_col(mapping = aes(x=fct_rev(fct_reorder(title, numberofdays)), y=numberofdays, fill=numberofdays)) +
  labs(title = "Top 10 Titles", x=NULL, y="Number of days" ) +
  theme(legend.position = "none") + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  geom_text(aes(x= title, y = numberofdays, label = numberofdays, vjust = 0))


View(subset(netflixDataset,title == "Ozark"))