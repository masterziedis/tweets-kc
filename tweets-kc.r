###--------------------------------------------------
### Twitter analysis
###--------------------------------------------------

## Get the data


## Load packages
library(ggplot2)
library(lubridate)
library(scales)

## Set working directory and load data
setwd("~/OneDrive/Vault/Vlaamse Overheid/Data Science/tweets-kc/data")
tweets <- read.csv("tweets.csv", stringsAsFactors = FALSE)

## Convert timestamp from string to datetime
tweets$timestamp <- ymd_hms(tweets$timestamp)
tweets$timestamp <- with_tz(tweets$timestamp, "Europe/Brussels")

## Tweets by Year, Month, and Day
ggplot(data = tweets, aes(x = timestamp)) +
  geom_histogram(aes(fill = ..count..)) +
  theme(legend.position = "none") +
  xlab("Jaar") + ylab("Aantal tweets") + 
  scale_fill_gradient(low = "lightgray", high = "black")

ggplot(data = tweets, aes(x = year(timestamp))) +
  geom_histogram(breaks = seq(2007.5, 2015.5, by =1), aes(fill = ..count..)) +
  theme(legend.position = "none") +
  xlab("Jaar") + ylab("Aantal tweets") + 
  scale_fill_gradient(low = "lightgray", high = "black")

ggplot(data = tweets, aes(x = wday(timestamp, label = TRUE))) +
  geom_bar(aes(fill = ..count..)) +
  theme(legend.position = "none") +
  xlab("Dag van de Week") + ylab("Aantal tweets") + 
  scale_fill_gradient(low = "lightgray", high = "black")

ggplot(data = tweets, aes(x = month(timestamp, label = TRUE))) +
  geom_bar(aes(fill = ..count..)) +
  theme(legend.position = "none") +
  xlab("Maand") + ylab("Aantal tweets") + 
  scale_fill_gradient(low = "lightgray", high = "black")

## Tweets by hour
tweets$timeonly <- as.numeric(tweets$timestamp - trunc(tweets$timestamp, "days"))
tweets[(minute(tweets$timestamp) == 0 & second(tweets$timestamp) == 0),11] <- NA
mean(is.na(tweets$timeonly))
class(tweets$timeonly) <- "POSIXct"
ggplot(data = tweets, aes(x = timeonly)) +
  geom_histogram(aes(fill = ..count..)) +
  theme(legend.position = "none") +
  xlab("Tijdstip") + ylab("Aantal tweets") + 
  scale_x_datetime(breaks = date_breaks("3 hours"), 
                   labels = date_format("%H:00")) +
  scale_fill_gradient(low = "lightgray", high = "black")

## Late night tweets
latenighttweets <- tweets[(hour(tweets$timestamp) < 6),]
ggplot(data = latenighttweets, aes(x = timestamp)) +
  geom_histogram(aes(fill = ..count..)) +
  theme(legend.position = "none") +
  xlab("Tijdstip") + ylab("Aantal tweets") + ggtitle("Late Night Tweets") +
  scale_fill_gradient(low = "lightgray", high = "black")

## Hashtags
ggplot(tweets, aes(factor(grepl("#", tweets$text)))) +
  geom_bar(fill = "darkgray") + 
  theme(legend.position="none", axis.title.x = element_blank()) +
  ylab("Aantal tweets") + 
  ggtitle("Tweets met Hashtags") +
  scale_x_discrete(labels=c("Zonder #", "Met #"))

## Retweets
ggplot(tweets, aes(factor(!is.na(retweeted_status_id)))) +
  geom_bar(fill = "darkgray") + 
  theme(legend.position="none", axis.title.x = element_blank()) +
  ylab("Aantal tweets") + 
  ggtitle("Retweeted Tweets") +
  scale_x_discrete(labels=c("Tweet", "Tweettweet"))

## Reply
ggplot(tweets, aes(factor(!is.na(in_reply_to_status_id)))) +
  geom_bar(fill = "darkgray") + 
  theme(legend.position="none", axis.title.x = element_blank()) +
  ylab("Aantal tweets") + 
  ggtitle("Replied Tweets") +
  scale_x_discrete(labels=c("Geen reply", "Replied tweets"))

## Putting it all together
tweets$type <- "tweet"
tweets[(!is.na(tweets$retweeted_status_id)),12] <- "RT"
tweets[(!is.na(tweets$in_reply_to_status_id)),12] <- "reply"
tweets$type <- as.factor(tweets$type)
tweets$type = factor(tweets$type,levels(tweets$type)[c(3,1,2)])

ggplot(data = tweets, aes(x = timestamp, fill = type)) +
  geom_histogram() +
  xlab("Time") + ylab("Number of tweets") +
  scale_fill_manual(values = c("#e5ffff", "darkgrey", "black"))

## Characters in tweet
tweets$charsintweet <- sapply(tweets$text, function(x) nchar(x))
ggplot(data = tweets, aes(x = charsintweet)) +
  geom_histogram(aes(fill = ..count..), binwidth = 8) +
  theme(legend.position = "none") +
  xlab("Tekens per Tweet") + ylab("Aantal tweets") + 
  scale_fill_gradient(low = "lightgray", high = "black")

## Index tweets with more then 140 Characters
tweets[(tweets$charsintweet > 140),]

