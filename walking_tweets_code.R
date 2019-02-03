setwd("D:/wohl")
library(twitteR)
library(ROAuth)
library(plyr)
library(dplyr)
library(stringr)
library(ggplot2)
library(httr)
library(wordcloud)
library(sentiment)
library(RCurl)
library(syuzhet)
oauth_endpoint(authorize = "https://api.twitter.com/oauth",
             access = "https://api.twitter.com/oauth/access_token")

download.file(url='http://curl.haxx.se/ca/cacert.pem', destfile='cacert.pem')
reqURL <- 'https://api.twitter.com/oauth/request_token'
accessURL <- 'https://api.twitter.com/oauth/access_token'
authURL <- 'https://api.twitter.com/oauth/authorize'

consumerKey="S0mKzlIa3teOHHeoCBc9HTxi0"
consumerSecret="zbA0jn1It2j0Huvy9Q5RTaxdZMGZYQ9CA5k4b6VCwYqfw1w8Xk"
accesstoken="91547858-5X4nm2wm3kYW1nxl3va5yFMyM5E6Pb3dntWrDEmwC"
accesssecret="bF6BnV9ncgCLz8dVtelQLxbOJSx5YrAIORPHzRl8GOQox"

Cred <- OAuthFactory$new(consumerKey=consumerKey,
                         consumerSecret=consumerSecret,
                         requestURL=reqURL,
                         accessURL=accessURL,
                         authURL=authURL)
Cred$handshake(cainfo = system.file('CurlSSL', 'cacert.pem', package = 'RCurl'))

save(Cred, file='twitter authentication.Rdata')

load('twitter authentication.Rdata') 

setup_twitter_oauth(consumer_key = consumerKey, consumer_secret = consumerSecret, access_token = accesstoken, access_secret = accesssecret)

some_tweets = searchTwitter("#Modi", n=100, since = "2017-10-27", lang= "en")

length.some_tweets <- length(some_tweets)
length.some_tweets

some_tweets.df <- ldply(some_tweets, function(t) t$toDataFrame())
write.csv(some_tweets.df, "Modi1_walking_tweets.csv")

some_txt = sapply(some_tweets, function(x) x$getText())

some_txt1 = gsub("(RT|via)((?:\\b\\W*@\\w+)+)","",some_txt)

some_txt2 = gsub("http[^[:blank:]]+", "", some_txt1)

some_txt3 = gsub("@\\w+","",some_txt2)

some_txt4 = gsub("[[:punct:]]", " ", some_txt3)

some_txt5 = gsub("[^[:alnum:]]", " ", some_txt4)

write.csv(some_txt5,"morning_walking_tweets1.csv")

library(tm)
some_txt6 <- Corpus(VectorSource(some_txt5))
some_txt6 <- tm_map(some_txt6, removePunctuation)
some_txt6 <- tm_map(some_txt6, content_transformer(tolower))
some_txt6 <- tm_map(some_txt6, removeWords, stopwords("english"))
some_txt6 <- tm_map(some_txt6, stripWhitespace)


pal <- brewer.pal(6,"Dark2")

wordcloud(some_txt6, min.freq = 2,  max.words = Inf, width=1000, height =1000,  random.order = FALSE, color=pal)


mysentiment <- get_nrc_sentiment(some_txt5)
SentimentScores <- data.frame(colSums(mysentiment[,]))
names(SentimentScores) <- "Score"
SentimentScores <- cbind("sentiment" = rownames(SentimentScores), SentimentScores)
rownames(SentimentScores) <- NULL
ggplot(data = SentimentScores, aes(x = sentiment, y = Score)) +
  geom_bar(aes(fill = sentiment), stat = "identity") +
  theme(legend.position = "none") +
  xlab("Sentiment") + ylab("Score") + ggtitle("Total Sentiment Score Based on morning walk Tweets")

