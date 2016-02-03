###--------------------------------------------------
### Twitter analysis
###--------------------------------------------------

## Source
\href{http://juliasilge.com/blog/Joy-to-the-World/}{Blog Julia}
\href{http://www.hln.be/hln/nl/948/Kunst-Literatuur/article/detail/2465468/2015/09/23/Dimitri-Verhulst-schreef-mooiste-literaire-zin-van-2014.dhtml}{Tzum award}


## Load packages
library(tm)
library(stringr)
library(wordcloud)

## Set working directory and load data
setwd("~/OneDrive/Vault/Vlaamse Overheid/Data Science/tweets-kc/data")
tweets <- read.csv("tweets.csv", stringsAsFactors = FALSE)

## Frequency of words
nohandles <- str_replace_all(tweets$text, "@\\w+", "")
wordCorpus <- Corpus(VectorSource(nohandles))
wordCorpus <- tm_map(wordCorpus, removePunctuation)
wordCorpus <- tm_map(wordCorpus, content_transformer(tolower))
wordCorpus <- tm_map(wordCorpus, removeWords, stopwords("english"))
wordCorpus <- tm_map(wordCorpus, removeWords, stopwords("dutch"))
wordCorpus <- tm_map(wordCorpus, removeWords, c("amp", "2yo", "3yo", "4yo")) #remove certain words
wordCorpus <- tm_map(wordCorpus, stripWhitespace)
wordCorpus <- tm_map(wordCorpus, stemDocument) #stemming: remove endings, not used

pal <- brewer.pal(9,"Greys")
pal <- pal[-(1:4)]
set.seed(123)
wordcloud(words = wordCorpus, scale=c(5,0.1), max.words=100, random.order=FALSE, 
          rot.per=0.35, use.r.layout=FALSE, colors=pal)

friends <- str_extract_all(tweets$text, "@\\w+")
namesCorpus <- Corpus(VectorSource(friends))
set.seed(146)
wordcloud(words = namesCorpus, scale=c(3,0.5), max.words=40, random.order=FALSE, 
          rot.per=0.10, use.r.layout=FALSE, colors=pal)

## Let's get Emotional
## Sentiment
library(syuzhet)
library(lubridate)
library(ggplot2)
library(scales)
library(reshape2)
library(dplyr )
mySentiment <- get_nrc_sentiment(tweets$text)
get_nrc_sentiment("Resistance is futile")
get_nrc_sentiment("Jouw kapsel, voor zover dat nog een kapsel mocht worden genoemd, had veel weg van zo'n in die dagen in zwang rakende ecologische tuin, waarin elke menselijke ingreep als een misdaad tegen de natuur werd beschouwd")

head(mySentiment)
tweets <- cbind(tweets, mySentiment)
sentimentTotals <- data.frame(colSums(tweets[,c(11:18)]))
names(sentimentTotals) <- "count"
sentimentTotals <- cbind("sentiment" = rownames(sentimentTotals), sentimentTotals)
rownames(sentimentTotals) <- NULL
ggplot(data = sentimentTotals, aes(x = sentiment, y = count)) +
  geom_bar(aes(fill = sentiment), stat = "identity") +
  theme(legend.position = "none") +
  xlab("Sentiment") + ylab("Aantal") + ggtitle("Tweets volgens sentiment")

## Sentiment over time
tweets$timestamp <- with_tz(ymd_hms(tweets$timestamp), "Europe/Brussels")
posnegtime <- tweets %>% 
  group_by(timestamp = cut(timestamp, breaks="2 months")) %>%
  summarise(negatief = mean(negative),
            positief = mean(positive)) %>% melt
names(posnegtime) <- c("timestamp", "sentiment", "meanvalue")
posnegtime$sentiment = factor(posnegtime$sentiment,levels(posnegtime$sentiment)[c(2,1)])

ggplot(data = posnegtime, aes(x = as.Date(timestamp), y = meanvalue, group = sentiment)) +
  geom_line(size = 2.5, alpha = 0.7, aes(color = sentiment)) +
  geom_point(size = 0.5) +
  ylim(0, NA) + 
  scale_colour_manual(values = c("springgreen4", "firebrick3")) +
  theme(legend.title=element_blank(), axis.title.x = element_blank()) +
  scale_x_date(breaks = date_breaks("10 months"), 
               labels = date_format("%Y-%b")) +
  ylab("Gemiddelde sentimentele score") + 
  ggtitle("Sentiment over de jaren")

## Sentiment on weekdays
tweets$weekday <- wday(tweets$timestamp, label = TRUE)
weeklysentiment <- tweets %>% group_by(weekday) %>% 
  summarise(anger = mean(anger), 
            anticipation = mean(anticipation), 
            disgust = mean(disgust), 
            fear = mean(fear), 
            joy = mean(joy), 
            sadness = mean(sadness), 
            surprise = mean(surprise), 
            trust = mean(trust)) %>% melt
names(weeklysentiment) <- c("weekday", "sentiment", "meanvalue")

ggplot(data = weeklysentiment, aes(x = weekday, y = meanvalue, group = sentiment)) +
  geom_line(size = 2.5, alpha = 0.7, aes(color = sentiment)) +
  geom_point(size = 0.5) +
  ylim(0, 0.6) +
  theme(legend.title=element_blank(), axis.title.x = element_blank()) +
  ylab("Gemiddelde sentimentele score") + 
  ggtitle("Sentiment volgens weekdag")

## Correlation months and feelings?
tweets$month <- month(tweets$timestamp, label = TRUE)
monthlysentiment <- tweets %>% group_by(month) %>% 
  summarise(anger = mean(anger), 
            anticipation = mean(anticipation), 
            disgust = mean(disgust), 
            fear = mean(fear), 
            joy = mean(joy), 
            sadness = mean(sadness), 
            surprise = mean(surprise), 
            trust = mean(trust)) %>% melt
names(monthlysentiment) <- c("month", "sentiment", "meanvalue")

ggplot(data = monthlysentiment, aes(x = month, y = meanvalue, group = sentiment)) +
  geom_line(size = 2.5, alpha = 0.7, aes(color = sentiment)) +
  geom_point(size = 0.5) +
  ylim(0, NA) +
  theme(legend.title=element_blank(), axis.title.x = element_blank()) +
  ylab("Gemiddelde sentimentele score") + 
  ggtitle("Sentiment doorheen het jaar")
  
