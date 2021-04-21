#Supervised vs. Unsupervised Learning

#install.packages("rtweet")
library(rtweet)

#Create Twitter API Token
token <- create_token(
  app = "",
  consumer_key = "",
  consumer_secret = "",
  access_token = "",
  access_secret = "")

#Pull most recent 3200 Tweets from McDonalds Twitter Page
rti = get_timeline("mcdonalds",token = token,n=3200)
tweets=data.frame(rti)
#-------------------------------------------------------x

#Pull In Data:
tweets<-read.csv("tweet_mcdonalds.csv")

#Unsupervised Learning
####################################Rule Association Learning################################
#install.packages("arules")
#install.packages("arulesViz")
#install.packages("stringr")
library(arules)
library(arulesViz)
library(stringr)

#Get some information on the number of words used in each tweet
word_count<-as.numeric(
  sapply(
    as.character(tweets$text),function(x){
      length(strsplit(x," ")[[1]])
      }
    )
  )

#Lets take a look!
hist(word_count)
summary(word_count)

#Prepare for market basket analysis!
words<-as.character(
  sapply(
    tweets$text,function(x){
      str_replace_all(x," ",",")  
    }
  )
)

##write.csv(words,"twitter_words.csv",quote=FALSE,row.names=TRUE)

#Run an algorithm to do the analysis and extract the rules out.
tr<-read.transactions("twitter_words.csv",format="basket",sep=",")
rules<-apriori(tr,parameter=list(supp=0.01,conf=.8))
topRules<-rules[1:100]

#Get some plots of the association rules
plot(topRules)
plot(topRules,method="graph")
plot(topRules,method="grouped")


####################################Clustering################################
rtclusters<-kmeans(tweets$retweet_count,3)
category<-rtclusters$cluster
tweets<-data.frame(tweets,rtCategory=as.factor(category))

hist(tweets$retweet_count,breaks = 1000, xlim = c(0,20))
# Not many retweets seen from Mcds

####################################Supervised Learning#######################
#Regression Problems
#install.packages("lexicon")
#install.packages("sentimentr")
#install.packages("parallel")
#install.packages("caret")
library(lexicon)
library(sentimentr)
library(parallel)
library(caret)

#First, we want to convert the tweet text to a sentiment score.  Pull up the 
#different lexicons
lexicons<- list(hash_sentiment_huliu,
                hash_sentiment_jockers,
                hash_sentiment_jockers_rinker,
                hash_sentiment_loughran_mcdonald,
                hash_sentiment_nrc,
                hash_sentiment_senticnet,
                hash_sentiment_sentiword,
                hash_sentiment_slangsd,
                hash_sentiment_socal_google)

#Now prepare to compute the sentiments
theText<-as.character(tweets$text)
theLexicon<-lexicons[[2]]

#We will now compute the lexicons. We will do this with sapply.
#If we use regular sapply, this will take some time running it on
#only a single core.

#Single-Core Processing
#textSentiments<-sapply(theText,function(i){sum(sentiment(theText[i],polarity_dt=hash_sentiment_jockers)$sentiment)})

#Alternativly, we can compute the sentiment for the 3200 tweets in parallel:
#Parallel Processing
clust<-makeCluster(detectCores(),type="FORK")
clusterEvalQ(clust,library(sentimentr))
clusterExport(clust,"theText")
clusterExport(clust,"theLexicon")
textSentiments<-parSapply(clust,1:length(theText),
                         function(x){
                           sum(sentiment(theText[x],polarity_dt=theLexicon)$sentiment)
                         })
stopCluster(clust)

#Now append the column of sentiments to the data frame.
tweets<-data.frame(tweets,textSentiments)

#Now let us run the regression problem with ordinary least squares.
#The formula:
theFormula<- retweet_count~textSentiments

#Basic OLS and Poisson Regression:
olsModel<- lm(theFormula,data=tweets)
poisModel<-glm(theFormula,family="poisson",data=tweets)
summary(poisModel)
summary(olsModel)


#Lower BIC is always preferred.
#Poisson performs better!
BIC(olsModel)
BIC(poisModel) #

#See what the residuals in each model look like:
olsresiduals<-olsModel$residuals
poisresiduals<-poisModel$residuals

theModel<-c(rep("OLS",length(olsresiduals)),rep("Poisson",length(poisresiduals)))
plotRes<-data.frame(model=theModel,residuals=c(olsresiduals,poisresiduals))
plot(plotRes$model,plotRes$residuals,outline=FALSE)
plot(plotRes$residuals)

#Classification Problem with Naive Bayes
library(e1071)

#Try to predict, based on text sentiment and favorite count, which 
#cluster the tweet belongs to using a naive bayes classifier.
classFormula<-rtCategory~textSentiments+favorite_count
nbc<-naiveBayes(classFormula,data=tweets)
testTweet<-tweets[1,]
predict(nbc,testTweet)

