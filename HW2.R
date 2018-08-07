getwd()
setwd('/Users/Scott/Documents/R')

#data.table library
library(data.table)

data <- fread("CSVDatasets/psychcentral_data.csv", sep=",", header=T, strip.white = T, na.strings = c("NA","NaN","","?")) 

#dplyr & tidytext
if(!require('dplyr')) install.packages('dplyr')
library(dplyr)

if(!require('tidytext')) install.packages('tidytext')
library(tidytext)

tidy_text <- data %>%
  unnest_tokens(word, q_content)

#Remove stop words
data(stop_words)
new_stop_words <- data.frame(c(stop_words, 'just', 'dont', 'like', 'think', 'know', 'want', 'will'))

tidy_text <- tidy_text %>%
  anti_join(new_stop_words)

#Count
tidy_text %>%
  count(word, sort = TRUE)

#ggplot
if(!require('ggplot2')) install.packages('ggplot2')
library(ggplot2)

tidy_text %>%
  count(word, sort = TRUE) %>%
  filter(n > 2000) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_bar(stat = "identity") +
  xlab(NULL) +
  coord_flip()

#SnowballC
if(!require('SnowballC')) install.packages('SnowballC')
library(SnowballC)
tidy_text <- data %>%
  unnest_tokens(word, q_content) %>%
  mutate(word = wordStem(word)) 

#remove stops
tidy_text <- tidy_text %>%
  anti_join(stop_words)

#Replot
tidy_text %>%
  count(word, sort = TRUE) %>%
  filter(n > 4000) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_bar(stat = "identity") +
  xlab(NULL) +
  coord_flip()

#wordcloud
if(!require('wordcloud')) install.packages('wordcloud')
library(wordcloud)

tidy_text %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 200))

#reshape2
if(!require('reshape2')) install.packages('reshape2')
library(reshape2)

#wordcloud by sentiment
tidy_text %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("#F8766D", "#00BFC4"),
                   max.words = 200)

#repeat for answers
answer_text <- data %>%
  unnest_tokens(word, answers)

#remove stops
answer_text <- answer_text %>%
  anti_join(stop_words)

#Replot
answer_text %>%
  count(word, sort = TRUE) %>%
  filter(n > 4000) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_bar(stat = "identity") +
  xlab(NULL) +
  coord_flip()

#repeat for answers stemmed
answer_text <- data %>%
  unnest_tokens(word, answers) %>%
  mutate(word = wordStem(word)) 

#remove stops
answer_text <- answer_text %>%
  anti_join(stop_words)


#Replot
answer_text %>%
  count(word, sort = TRUE) %>%
  filter(n > 4000) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_bar(stat = "identity") +
  xlab(NULL) +
  coord_flip()

#wordcloud
if(!require('wordcloud')) install.packages('wordcloud')
library(wordcloud)

tidy_text %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 200))

#reshape2
if(!require('reshape2')) install.packages('reshape2')
library(reshape2)

#wordcloud by sentiment
answer_text %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("#F8766D", "#00BFC4"),
                   max.words = 200)

#Professor code
if(!require('RTextTools')) install.packages('RTextTools')
library(RTextTools)
if(!require('tm')) install.packages('tm')
library(tm)
if(!require('topicmodels')) install.packages('topicmodels')
library(topicmodels)
if(!require('slam')) install.packages('slam')
library(slam)
data <- data[1:1000,] # We perform LDA on the rows 1 through 1000 in the data.
corpus <- Corpus(VectorSource(data$q_content), readerControl=list(language="en"))
dtm <- DocumentTermMatrix(corpus, control = list(stopwords = TRUE, minWordLength = 2, removeNumbers = TRUE, removePunctuation = TRUE,  stemDocument = TRUE))
rowTotals <- apply(dtm , 1, sum) #Find the sum of words in each Document
dtm.new   <- dtm[rowTotals> 0, ] #remove all docs without words
lda <- LDA(dtm.new, k = 5) # k is the number of topics to be found.

#bar plot
lda_td <- tidy(lda)
top_terms <- lda_td %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()

#k = 2
lda <- LDA(dtm.new, k = 2)
#bar plot
lda_td <- tidy(lda)
top_terms <- lda_td %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()

#k = 3
lda <- LDA(dtm.new, k = 3)
#bar plot
lda_td <- tidy(lda)
top_terms <- lda_td %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()

#k = 4
lda <- LDA(dtm.new, k = 4)
#bar plot
lda_td <- tidy(lda)
top_terms <- lda_td %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()

#k = 10
lda <- LDA(dtm.new, k = 10)
#bar plot
lda_td <- tidy(lda)
top_terms <- lda_td %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()


#Professor Code
data <- data[1:1000,] # We perform LDA on the rows 1 through 1000 in the data.
corpus <- Corpus(VectorSource(data$answers), readerControl=list(language="en"))
dtm <- DocumentTermMatrix(corpus, control = list(stopwords = TRUE, minWordLength = 2, removeNumbers = TRUE, removePunctuation = TRUE,  stemDocument = TRUE))
rowTotals <- apply(dtm , 1, sum) #Find the sum of words in each Document
dtm.new   <- dtm[rowTotals> 0, ] #remove all docs without words
lda <- LDA(dtm.new, k = 10) # k is the number of topics to be found.

#k = 10
#bar plot
lda_td <- tidy(lda)
top_terms <- lda_td %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()

#k = 2
lda <- LDA(dtm.new, k = 2)
#bar plot
lda_td <- tidy(lda)
top_terms <- lda_td %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()

#k = 8
lda <- LDA(dtm.new, k = 8)
#bar plot
lda_td <- tidy(lda)
top_terms <- lda_td %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()

#k = 11
lda <- LDA(dtm.new, k = 11)
#bar plot
lda_td <- tidy(lda)
top_terms <- lda_td %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()

#k = 14
lda <- LDA(dtm.new, k = 14)
#bar plot
lda_td <- tidy(lda)
top_terms <- lda_td %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()
