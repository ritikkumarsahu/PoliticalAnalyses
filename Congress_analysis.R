# Read file
CONG <- read.csv(file.choose(), header = T)
str(CONG)

# Build corpus_cong
library(tm)
corpus_cong <- iconv(CONG$tweet, to = 'UTF-8')
corpus_cong <- Corpus(VectorSource(corpus_cong))
inspect(corpus_cong[1:5])

# Clean text
corpus_cong <- tm_map(corpus_cong, tolower)
inspect(corpus_cong[1:5])

corpus_cong <- tm_map(corpus_cong, PlainTextDocument)

corpus_cong <- tm_map(corpus_cong, removePunctuation)
inspect(corpus_cong[1:5])

corpus_cong <- tm_map(corpus_cong, removeNumbers)
inspect(corpus_cong[1:5])

cleanset_cong <- tm_map(corpus_cong, removeWords, stopwords('english'))
inspect(cleanset_cong[1:5])

removeURL <- function(x) gsub('http[[:alnum:]]*', '', x)

cleanset_cong <- tm_map(cleanset_cong, content_transformer(removeURL))
inspect(cleanset_cong[1:5])

gsub("[^a-zA-Z]", "", cleanset_cong)

cleanset_cong <- tm_map(cleanset_cong, stripWhitespace)
inspect(cleanset_cong[1:5])


# Term document matrix
tdm_cong <- TermDocumentMatrix(cleanset_cong)
tdm_cong <- as.matrix(tdm_cong)

# Bar plot
w_cong <- rowSums(tdm_cong)
w_cong <- subset(w_cong, w_cong >= 25)
barplot(w_cong,
        las = 2,
        col = rainbow(50))

# Word cloud
library(wordcloud)
w_cong <- sort(rowSums(tdm_cong), decreasing = TRUE)
set.seed(222)
wordcloud(words = names(w_cong),
          freq = w_cong,
          max.words = 150,
          random.order = F,
          min.freq = 5,
          colors = brewer.pal(8, 'Dark2'),
          scale = c(5, 0.3),
          rot.per = 0.7)

library(wordcloud2)
w_cong <- data.frame(names(w_cong), w_cong)
colnames(w_cong) <- c('word', 'freq')
wordcloud2(w_cong,
           size = 0.7,
           shape = 'triangle',
           rotateRatio = 0.5,
           minSize = 1)


# Sentiment analysis
library(syuzhet)
library(lubridate)
library(ggplot2)
library(scales)
library(reshape2)
library(dplyr)

# Read file
CONG <- read.csv(file.choose(), header = T)
tweets_cong <- iconv(CONG$tweet, to = 'utf-8')

# Obtain sentiment scores
s_cong <- get_nrc_sentiment(tweets_cong)
head(s_cong)
tweets_cong[5]
sentiment_score_Congress = colSums(s_cong)
as.list(sentiment_score_Congress)
get_nrc_sentiment('mad')

# Bar plot
barplot(colSums(s_cong),
        las = 2,
        col = rainbow(10),
        ylab = 'Count',
        main = 'Sentiment Scores for Congress Tweets')
