####################################################
####################################################
####################################################
####################################################
####################################################

## Data Visualization
## Sentiment analysis of tweets 

####################################################
####################################################

## Loading the libraries

library(ggplot2)
library(lubridate)
?dplyr
library(scales)
library(tm)
library(stringr)
library(wordcloud)
library(RColorBrewer)
library(syuzhet)
library(reshape2)
library(dplyr)
library(twitteR)

############################################################

## Importing the tweets from dataset
tweet <- read.csv('modietweets.csv', header = T, sep = ";") 
class(tweet)

## Parsing the date and time of the tweets
tweet$`Created.At` <- dmy_hm(tweet$`Created.At`)
## Allocating to one timezone
tweet$`Created.At` <- with_tz(tweet$`Created.At`, "Asia/Kolkata")

## Plotting the tweets by month
ggplot(data = tweet, aes(x = month(`Created.At`, label = TRUE))) +
  geom_bar(aes(fill = ..count..)) +
  theme(legend.position = "none") +
  xlab("Month") + ylab("Number of tweets") +
  scale_fill_gradient(low = "blue", high = "red")

## Grouping tweets by time
tweet$timeonly <- as.numeric(tweet$`Created.At` - trunc(tweet$`Created.At`, "days"))
class(tweet$timeonly) <- "POSIXct"

## Plotting by time
ggplot(data = tweet, aes(x = timeonly)) +
  geom_histogram(aes(fill = ..count..)) +
  theme(legend.position = "none") +
  xlab("Time") + ylab("Number of tweets") + 
  scale_x_datetime(breaks = date_breaks("3 hours"), 
                   labels = date_format("%H:00")) +
  scale_fill_gradient(low = "blue", high = "red")

## CLeaning the text
nohandles <- str_replace_all(tweet$Text, "@\\w+", "") ## Removing symbols

## Creating the corpus
wordCorpus <- Corpus(VectorSource(nohandles)) 
wordCorpus <- tm_map(wordCorpus, removePunctuation) 
wordCorpus <- tm_map(wordCorpus, content_transformer(tolower)) #converted to lower case
wordCorpus <- tm_map(wordCorpus, removeWords, stopwords("english")) #remove stopwords
wordCorpus <- tm_map(wordCorpus, removeWords, c("amp", "2yo", "3yo", "4yo"))
wordCorpus <- tm_map(wordCorpus, stripWhitespace)

## Plotting th corpus
pal <- brewer.pal(9,"YlGnBu")
pal <- pal[-(1:4)]
set.seed(123)

## Creating a wordcloud
wordcloud(words = wordCorpus, scale=c(10,0.3), max.words=100, random.order=FALSE, 
          rot.per=0.35, use.r.layout=FALSE, colors=pal)

## Cleaning text
tweet$clean_text <- str_replace_all(tweet$Text, "@\\w+", "")

## Sentiment analysis
Sentiment <- get_nrc_sentiment(tweet$clean_text)
tweets_senti <- cbind(tweet, Sentiment)
View(tweets_senti)

## Total of the sentiment weights
sentimentTotals <- data.frame(colSums(tweets_senti[,c(15:22)]))
names(sentimentTotals) <- "count"

## Joining the datasets
sentimentTotals <- cbind("sentiment" = rownames(sentimentTotals), sentimentTotals)
rownames(sentimentTotals) <- NULL

## Plotting the sentiment scores
ggplot(data = sentimentTotals, aes(x = sentiment, y = count)) +
  geom_bar(aes(fill = sentiment), stat = "identity") +
  theme(legend.position = "none") +
  xlab("Sentiment") + ylab("Total Count") + ggtitle("Total Sentiment Score for All Tweets")

## Positive and negative tweets
posnegtime <- tweets_senti %>% 
  group_by(created = cut(`Created.At`, breaks="10 hour")) %>%
  summarise(negative = mean(negative),
            positive = mean(positive)) %>% melt

## Naming the dataframe
names(posnegtime) <- c("timestamp", "sentiment", "meanvalue")
posnegtime$sentiment = factor(posnegtime$sentiment,levels(posnegtime$sentiment)[c(2,1)])

## Plotting the tweets
ggplot(data = posnegtime, aes(x = as.Date(timestamp), y = meanvalue, group = sentiment)) +
  geom_line(size = 1.5, alpha = 0.7, aes(color = sentiment)) +
  geom_point(size = 0.3) +
  ylim(0, NA) + 
  scale_colour_manual(values = c("green", "red")) +
  theme(legend.title=element_blank(), axis.title.x = element_blank()) +
  scale_x_time(breaks = waiver(), minor_breaks = waiver(), 
               labels = date_format("%h-%m")) +
  ylab("Average sentiment score") + 
  ggtitle("Sentiment Over Time")

## Grouping by emotion

tweets_senti$day <- wday(tweets_senti$`Created.At`, label = TRUE)
dailysentiment <- tweets_senti %>% group_by(day) %>% 
  summarise(anger = mean(anger), 
            anticipation = mean(anticipation), 
            disgust = mean(disgust), 
            fear = mean(fear), 
            joy = mean(joy), 
            sadness = mean(sadness), 
            surprise = mean(surprise), 
            trust = mean(trust)) %>% melt

## Variation in sentiment by day
names(dailysentiment) <- c("day", "sentiment", "meanvalue")

## Plotting the emotion variation by day
ggplot(data = dailysentiment, aes(x = day, y = meanvalue, group = sentiment)) +
  geom_line(size = 2.5, alpha = 0.7, aes(color = sentiment)) +
  geom_point(size = 0.5) +
  ylim(0, NA) +
  theme(legend.title=element_blank(), axis.title.x = element_blank()) +
  ylab("Average sentiment score") + 
  ggtitle("Sentiment During the Year")


####################################################
####################################################
####################################################
####################################################
