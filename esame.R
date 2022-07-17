library("tidyverse")
library(hrbrthemes)
library(wordcloud)
library(RColorBrewer)
library(gganimate)

#----------------------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------------------

#Import netflix dataset

netflixDataset = read_csv("C:\\Users\\Alessio\\Documents\\Università\\Data Science\\Netflix R Project\\netflix_titles.csv")
View(netflixDataset)

#removing unused content

netflixDataset = 
  netflixDataset %>%
  select(show_id,type,title,country,release_year,rating,duration,listed_in)
#----------------------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------------------

#Plot movies by type

movietype = 
  netflixDataset %>%
  count(type) %>%
  rename(count = n) %>%
  arrange(-count)

#View(movietype)

movies = ggplot(data = movietype)+
  geom_col(mapping = aes(type,count,fill=type))+
  labs(title = "Movies Type", x="Type", y="Count")

#----------------------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------------------

#Plot years with most released films

netflixFiltered = 
  netflixDataset %>%
  drop_na(release_year) %>%
  filter(between(release_year,1990,2018))


topyears = 
  netflixFiltered %>%
  count(release_year) %>%
  rename(count = n) %>%
  arrange(-count) 

#View(topyears)

ggplot(data = topyears)+
  geom_col(aes(as.character(release_year),count,fill=as.character(release_year)))+
  coord_flip()+
  labs(x="Release Year",y="Count",fill="Release Year")

ggplot(data = topyears)+
  geom_area(aes(release_year,count),fill="lightgreen")+
  scale_x_continuous(breaks = seq(1990, 2018, 2),limits=c(1990, 2018))+
  labs(x="Release Year",y="Count",title = "Film released per year")

#----------------------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------------------

#Select Top 10 Country with major production

countries = 
  netflixDataset %>%
  select(country) %>%
  drop_na(country) %>%
  mutate(country = strsplit(as.character(country), ", ")) %>%
  unnest(country) %>%
  count(country) %>%
  rename(count= n) %>%
  arrange(-count) %>%
  head(10)

#View(countries)

ggplot(data = countries)+
  geom_col(aes(fct_reorder(country, count),count,fill = country))+
  labs(title = "Top 10 Production Country", x="Countries", y="Count" ) +
  scale_fill_brewer(palette = "Set3")+
  coord_flip()

#----------------------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------------------

#Top 25 Imdb rated films

#Importing two datasets from imdb
imdbRatings = read_csv("C:\\Users\\Alessio\\Documents\\Università\\Data Science\\Netflix R Project\\imdb\\IMDb ratings.csv")
imdbRatings = select(imdbRatings,weighted_average_vote)
imdbMovies = read_csv("C:\\Users\\Alessio\\Documents\\Università\\Data Science\\Netflix R Project\\imdb\\IMDb movies.csv")
imdbMovies = select(imdbMovies,title,year,genre)


ratings = data.frame(
  title= select(imdbMovies,title),
  releaseYear = select(imdbMovies,year),
  rating = select(imdbRatings,weighted_average_vote),
  genre = select(imdbMovies,genre))

ratings = 
  ratings %>%
  distinct(title, year,weighted_average_vote) %>%
  drop_na()

joinedDataset = 
  netflixDataset %>%
  inner_join(ratings) %>%
  filter(duplicated(show_id) == FALSE)

top25ratedmovies = 
  joinedDataset %>%
  arrange(-weighted_average_vote) %>%
  head(25)

#View(top25ratedmovies)


ggplot(data = top25ratedmovies)+
  geom_point(aes(fct_reorder(title,weighted_average_vote),weighted_average_vote),col="tomato2",size=3)+
  geom_segment(aes(x= title, xend = title,y=min(weighted_average_vote),yend=max(weighted_average_vote)),linetype="dashed",size=0.1)+
  labs(title="Top 25 Imbd Rated Movies",x="Imdb Vote", y="Movies Titles" )+
  coord_flip()

#----------------------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------------------

#Pie Chart of top 5 rated country

top5ratedCountries = 
  joinedDataset %>%
  arrange(-weighted_average_vote) %>%
  drop_na() %>%
  mutate(country = strsplit(as.character(country), ", ")) %>%
  unnest(country) %>%
  count(country) %>%
  rename(count = n) %>%
  arrange(-count) %>%
  head(5) %>%
  mutate(prop = count / sum(top5ratedCountries$count) * 100) %>%
  mutate(ypos = cumsum(prop)-0.7*prop)

#View(top5ratedCountries)

ggplot(data = top5ratedCountries) +
  geom_bar(aes(x="",y=prop,fill=country),stat="identity",width=1,color="white")+
  coord_polar("y",start=0)+
  theme_void()+
  scale_fill_brewer(palette = "Dark2")+
  labs(title="Top 5 Imdb Rated countries",fill = "Countries")

#----------------------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------------------

#Movie rating analysis

movierating = 
  netflixDataset %>%
  select(rating) %>%
  count(rating) %>%
  drop_na() %>%
  rename(count = n) %>%
  arrange(-count)

#View(movierating)

ggplot(data = movierating)+
  geom_col(aes(fct_rev(fct_reorder(rating,count)),count,fill=rating),width = 0.8)+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))+
  ggtitle("Movie category ratings")+
  labs(x="Ratings",y="Count")

#----------------------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------------------

#Movie duration analysis

durationAnalysis = 
  netflixDataset %>%
  filter(!str_detect(duration,"Season")) %>%
  mutate(duration = str_replace(as.character(duration)," min",'')) 


durationAnalysis = 
  durationAnalysis %>%
  arrange(as.numeric(duration))

#View(durationAnalysis)


ggplot(data=durationAnalysis)+
  geom_density(aes(x=as.numeric(duration)), fill="#69b3a2", color="#e9ecef", alpha=0.8)+
  ggtitle("Analysis of the duration of the movies")+
  theme_ipsum()+
  scale_x_continuous(breaks = seq(0, 200, 25),limits=c(0, 200))+
  labs(x="Duration",y="Density")+ 
  theme(
    panel.background = element_rect(fill='transparent'), #transparent panel bg
    plot.background = element_rect(fill='transparent', color=NA), #transparent plot bg
    legend.background = element_rect(fill='transparent'), #transparent legend bg
    legend.box.background = element_rect(fill='transparent') #transparent legend panel
  )

#----------------------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------------------

#Word cloud of genre
listgenre = 
  netflixDataset %>%
  select(listed_in) %>%
  mutate(listed_in = strsplit(as.character(listed_in), ", ")) %>%
  unnest(listed_in) %>%
  count(listed_in)

View(listgenre)

wordcloud(size=4,words=listgenre$listed_in,listgenre$n, min.freq = 1, max.words = 60, random.order = FALSE, rot.per = 0.50,colors=brewer.pal(8,"Dark2"))
