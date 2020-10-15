#connect all libraries
library(twitteR)
library(ROAuth)
library(plyr)
library(dplyr)
library(stringr)
library(ggplot2)
library(openssl)
library(httpuv)
library(base64enc)
library(tmap)
library(tm)
library(syuzhet)
library(wordcloud)
library(SnowballC)
library(stringi)
library(topicmodels)


download.file(url='http://curl.haxx.se/ca/cacert.pem', destfile='cacert.pem')
#options(httr_oauth_cache=T)
devtools::install_version("httr", version="1.0.0", repos="http://cran.us.r-project.org")
api_key = "8zaEuPipuNl4T9GLBHc3qnL2z" # your api_key
api_secret = "fpYvLJT7L0q6TPQ7KOI94JSEgifHV5VHqHWX94SciaGNOCLXCz" # your api_secret 
access_token = "2599960604-dK8qZvGx4Y1MJPwkp5YAwncfBFKFqDpcNe0O9pD" # your access_token 
access_token_secret = "ifC7bA0OQYo1s8AOmF7hFMMZdT8Up21tAkWxEdwJKNCgv" # your access_token_sceret 
credential<-OAuthFactory$new(consumerKey=api_key,
                             consumerSecret=api_secret,
                             requestURL="https://api.twitter.com/oauth/request_token",
                             accessURL="https://api.twitter.com/oauth/access_token",
                             authURL="https://api.twitter.com/oauth/authorize")

credential$handshake()

setup_twitter_oauth(api_key, api_secret, access_token, access_token_secret)
origop <- options("httr_oauth_cache")
options(httr_oauth_cache = TRUE)

tweets_g <- searchTwitter("#google", n=1000,lang = "en")
tweets_a <- searchTwitter("#amazon", n=1000,lang = "en")
tweets_f <- searchTwitter("#facebook", n=1000,lang = "en")
tweets_tech <- searchTwitter("#technology", n=1000,lang = "en")

amazon_tweets <- twListToDF(tweets_a)
google_tweets <- twListToDF(tweets_g)
facebook_tweets <- twListToDF(tweets_f)
tech_tweets <- twListToDF(tweets_tech)

google_text<- google_tweets$text
amazon_text<- amazon_tweets$text
facebook_text<- facebook_tweets$text
tech_text<- tech_tweets$text

#convert all text to lower case
google_text<- tolower(google_text)
amazon_text<- tolower(amazon_text)
facebook_text<- tolower(facebook_text)
tech_text<- tolower(tech_text)

# Replace blank space (“rt”)
google_text <- gsub("rt", "", google_text)
amazon_text <- gsub("rt", "", amazon_text)
facebook_text <- gsub("rt", "", facebook_text)
tech_text <- gsub("rt", "", tech_text)

# Replace @UserName
google_text <- gsub("@\\w+", "", google_text)
amazon_text <- gsub("@\\w+", "", amazon_text)
facebook_text <- gsub("@\\w+", "", facebook_text)
tech_text <- gsub("@\\w+", "", tech_text)


# Remove punctuation
google_text <- gsub("[[:punct:]]", "", google_text)
amazon_text <- gsub("[[:punct:]]", "", amazon_text)
facebook_text <- gsub("[[:punct:]]", "", facebook_text)
tech_text <- gsub("[[:punct:]]", "", tech_text)


# Remove links
google_text <- gsub("http\\w+", "", google_text)
amazon_text <- gsub("http\\w+", "", amazon_text)
facebook_text <- gsub("http\\w+", "", facebook_text)
tech_text <- gsub("http\\w+", "", tech_text)

# Remove tabs
google_text <- gsub("[ |\t]{2,}", "", google_text)
amazon_text <- gsub("[ |\t]{2,}", "", amazon_text)
facebook_text <- gsub("[ |\t]{2,}", "", facebook_text)
tech_text <- gsub("[ |\t]{2,}", "", tech_text)


# Remove blank spaces at the beginning
google_text <- gsub("^ ", "", google_text)
amazon_text <- gsub("^ ", "", amazon_text)
facebook_text <- gsub("^ ", "", facebook_text)
tech_text <- gsub("^ ", "", tech_text)

# Remove blank spaces at the end
google_text <- gsub(" $", "", google_text)
amazon_text <- gsub(" $", "", amazon_text)
facebook_text <- gsub(" $", "", facebook_text)
tech_text <- gsub(" $", "", tech_text)



#generate wordcloud
wordcloud(google_text,min.freq = 10,colors=brewer.pal(8, "Dark2"),random.color = TRUE,max.words = 500)
wordcloud(amazon_text,min.freq = 10,colors=brewer.pal(8, "Dark2"),random.color = TRUE,max.words = 500)
wordcloud(facebook_text,min.freq = 10,colors=brewer.pal(8, "Dark2"),random.color = TRUE,max.words = 500)
wordcloud(tech_text,min.freq = 10,colors=brewer.pal(8, "Dark2"),random.color = TRUE,max.words = 500)

#getting emotions using in-built function
mysentiment_google<-get_nrc_sentiment((google_text))
mysentiment_amazon<-get_nrc_sentiment((amazon_text))
mysentiment_facebook<-get_nrc_sentiment((facebook_text))
mysentiment_tech<-get_nrc_sentiment((tech_text))
#calculationg total score for each sentiment
Sentimentscores_google<-data.frame(colSums(mysentiment_google[,]))
Sentimentscores_amazon<-data.frame(colSums(mysentiment_amazon[,]))
Sentimentscores_facebook<-data.frame(colSums(mysentiment_facebook[,]))
Sentimentscores_tech<-data.frame(colSums(mysentiment_tech[,]))

names(Sentimentscores_google)<-"Score"
Sentimentscores_google<-cbind("sentiment"=rownames(Sentimentscores_google),Sentimentscores_google)
rownames(Sentimentscores_google)<-NULL

names(Sentimentscores_amazon)<-"Score"
Sentimentscores_amazon<-cbind("sentiment"=rownames(Sentimentscores_amazon),Sentimentscores_amazon)
rownames(Sentimentscores_amazon)<-NULL

names(Sentimentscores_facebook)<-"Score"
Sentimentscores_facebook<-cbind("sentiment"=rownames(Sentimentscores_facebook),Sentimentscores_facebook)
rownames(Sentimentscores_facebook)<-NULL

names(Sentimentscores_tech)<-"Score"
Sentimentscores_tech<-cbind("sentiment"=rownames(Sentimentscores_tech),Sentimentscores_tech)
rownames(Sentimentscores_tech)<-NULL

#*************************************************************************************

#plotting the sentiments with scores
ggplot(data=Sentimentscores_google,aes(x=sentiment,y=Score))+geom_bar(aes(fill=sentiment),stat = "identity")+
  theme(legend.position="none")+
  xlab("Sentiments")+ylab("scores")+ggtitle("Sentiments of people behind the tweets on tech giant GOOGLE")


ggplot(data=Sentimentscores_amazon,aes(x=sentiment,y=Score))+geom_bar(aes(fill=sentiment),stat = "identity")+
  theme(legend.position="none")+
  xlab("Sentiments")+ylab("scores")+ggtitle("Sentiments of people behind the tweets on ecomerce giant AMAZON")


ggplot(data=Sentimentscores_facebook,aes(x=sentiment,y=Score))+geom_bar(aes(fill=sentiment),stat = "identity")+
  theme(legend.position="none")+
  xlab("Sentiments")+ylab("scores")+ggtitle("Sentiments of people behind the tweets on Social Netwoking site FACEBOOK")


ggplot(data=Sentimentscores_tech,aes(x=sentiment,y=Score))+geom_bar(aes(fill=sentiment),stat = "identity")+
  theme(legend.position="none")+
  xlab("Sentiments")+ylab("scores")+ggtitle("Sentiments of people behind the tweets on tech as a whole")



