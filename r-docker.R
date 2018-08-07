# Step 1: Go to http://play-with-docker and create an instance
# Step 2: Run "docker run -d -p 8787:8787 rocker/tidyverse"
# Step 3: Click 8888 link to open in browser. Copy token and press ok.
# Step 4: Download this file by running: download.file("https://gist.githubusercontent.com/wesslen/ae9aca04b491a064764b13239fb17489/raw/627631f9f73e5c51a2eb2144f1a82f2f1a105b6c/r-docker.R", destfile = "r-docker.R")

library(tidyverse)

# load tweets
file <- "https://github.com/wesslen/summer-2017-social-media-workshop/raw/master/data/CharlotteTweets20Sample.csv"
tweets <- read_csv(file)

# counts

tweets %>% 
  group_by(actor.location.displayName) %>%
  summarise(Count=n()) %>%
  arrange(desc(Count)) %>%
  head(n=10)

# tidy text

install.packages("tidytext")
library(tidytext)

tidy_tweets <- tweets %>% 
  unnest_tokens(word, body)

# count words

counts <- tidy_tweets %>% 
  count(word, sort = TRUE)

head(counts, n = 10)

# remove stop words

data("stop_words")
cleaned_tweets <- tidy_tweets %>%
  anti_join(stop_words) %>%
  count(word, sort = TRUE) 

head(cleaned_tweets, n = 10)

# count sentiment net scores

bing <- get_sentiments("bing")

sentiment <- tidy_tweets %>%
  inner_join(bing) %>%
  count(day = as.Date(postedTime), sentiment, count = n()) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = (positive - negative) / count)

# plot scores

ggplot(sentiment, aes(day, sentiment)) +
  geom_point() +
  geom_smooth(method = "loess", size = 1.5, span = 0.1) +
  labs(title = "Sentiment Analysis", subtitle = "Net Sentiment Score as Percent of Tweets", x = "Day", y = "Sentiment")

# locate beer-related tweets with leaflet

install.packages("leaflet")
library(leaflet)

query <- "beer"

t <- subset(tweets[grep(query,tweets$body, ignore.case = TRUE),], !is.na(point_long))

leaflet(t) %>%
  addTiles() %>%
  addCircleMarkers(lng=t$point_lat, lat=t$point_long,   popup = t$body, 
                   stroke = FALSE, fillOpacity = 0.5, radius = 10, clusterOptions = markerClusterOptions()
  )