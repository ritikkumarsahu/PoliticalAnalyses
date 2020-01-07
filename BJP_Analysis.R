# Read file
BJP <- read.csv(file.choose(), header = T)
str(BJP)

# Build corpus_bjp
library(tm)
corpus_bjp <- iconv(BJP$tweet, to = 'UTF-8')
corpus_bjp <- Corpus(VectorSource(corpus_bjp))
inspect(corpus_bjp[1:5])

# Clean text
corpus_bjp <- tm_map(corpus_bjp, tolower)
inspect(corpus_bjp[1:5])

corpus_bjp <- tm_map(corpus_bjp, PlainTextDocument)

corpus_bjp <- tm_map(corpus_bjp, removePunctuation)
inspect(corpus_bjp[1:5])

corpus_bjp <- tm_map(corpus_bjp, removeNumbers)
inspect(corpus_bjp[1:5])

cleanset_bjp <- tm_map(corpus_bjp, removeWords, stopwords('english'))
inspect(cleanset_bjp[1:5])

removeURL <- function(x) gsub('http[[:alnum:]]*', '', x)

cleanset_bjp <- tm_map(cleanset_bjp, content_transformer(removeURL))
inspect(cleanset_bjp[1:5])

gsub("[^a-zA-Z]","",cleanset_bjp)

cleanset_bjp <- tm_map(cleanset_bjp, stripWhitespace)
inspect(cleanset_bjp[1:5])


# Term document matrix
tdm_bjp <- TermDocumentMatrix(cleanset_bjp)
tdm_bjp <- as.matrix(tdm_bjp)

# Bar plot
w_bjp <- rowSums(tdm_bjp)
w_bjp <- subset(w_bjp, w_bjp>=160)
barplot(w_bjp,
        las = 2,
        col = rainbow(50))

# Word cloud
library(wordcloud)
w_bjp <- sort(rowSums(tdm_bjp), decreasing = TRUE)
set.seed(222)
wordcloud(words = names(w_bjp),
          freq = w_bjp,
          max.words = 150,
          random.order = F,
          min.freq = 5,
          colors = brewer.pal(8, 'Dark2'),
          scale = c(5, 0.3),
          rot.per = 0.7)

library(wordcloud2)
w_bjp <- data.frame(names(w_bjp), w_bjp)
colnames(w_bjp) <- c('word', 'freq')
wordcloud2(w_bjp,
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
BJP <- read.csv(file.choose(), header = T)
tweets_bjp <- iconv(BJP$tweet, to = 'utf-8')

# Obtain sentiment scores
s_bjp <- get_nrc_sentiment(tweets_bjp)
head(s_bjp)
tweets_bjp[4]
sentiment_score_BJP = colSums(s_bjp)
as.list(sentiment_score_BJP)
get_nrc_sentiment('killing')

# Bar plot
barplot(colSums(s_bjp),
        las = 2,
        col = rainbow(10),
        ylab = 'Count',
        main = 'Sentiment Scores for BJP Tweets')
