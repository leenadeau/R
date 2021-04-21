# ML Methods

# Load Data
attach(xg2)
m10_data <- xg2

summary(m10_data)
head(m10_data$retweet_count)
head(m10_data)

# Clustering
rtclusters<-kmeans(na.omit(m10_data$retweet_count),3)

rtclusters<-kmeans(m10_data$retweet_count,3)
category<-rtclusters$cluster
m10_data<-data.frame(m10_data,rtCategory=as.factor(category))
head(m10_data)

# Supervised Learning
# Regression Problems
library(lexicon)
library(sentimentr)
library(parallel)
library(caret)

# 1: Convert the tweet text to a sentiment score. Pull up thedifferent lexicons
lexicons<- list(hash_sentiment_huliu,
                hash_sentiment_jockers,
                hash_sentiment_jockers_rinker,
                hash_sentiment_loughran_mcdonald,
                hash_sentiment_nrc,
                hash_sentiment_senticnet,
                hash_sentiment_sentiword,
                hash_sentiment_slangsd,
                hash_sentiment_socal_google)

# Prepare to compute the sentiments
theText<-as.character(m10_data$text)
theLexicon<-lexicons[[2]]

# Parallel Processing
clust<-makeCluster(detectCores(),type="FORK")
clusterEvalQ(clust,library(sentimentr))
clusterExport(clust,"theText")
clusterExport(clust,"theLexicon")
#textSentiments<-parSapply(clust,1:length(theText),
#                          function(x){
#                            sum(sentiment(theText[x],polarity_dt=theLexicon)$sentiment)
#                          })
#stopCluster(clust)

## Long time to run ^^^

# Append the column of sentiments to the data frame.
m10_data<-data.frame(m10_data,textSentiments)

# Run the regression problem with ordinary least squares.
#The formula:
theFormula<- retweet_count~textSentiments

# Basic OLS and Poisson Regression:
olsModel<- lm(theFormula,data=m10_data)
poisModel<-glm(theFormula,family="poisson",data=m10_data)
summary(poisModel)
summary(olsModel)

# Lower BIC preferred
BIC(olsModel) # OLS Performed better
BIC(poisModel) 

# See what the residuals in each model look like:
olsresiduals<-olsModel$residuals
poisresiduals<-poisModel$residuals

theModel<-c(rep("OLS",length(olsresiduals)),rep("Poisson",length(poisresiduals)))
plotRes<-data.frame(model=theModel,residuals=c(olsresiduals,poisresiduals))
plot(plotRes$model,plotRes$residuals,outline=FALSE)

# Predict, based on text sentiment and favorite count, which 
# cluster the tweet belongs to using a naive bayes classifier.
classFormula<-rtCategory~textSentiments+favorite_count
nbc<-naiveBayes(classFormula,data=tweets)
testTweet<-tweets[1,]
predict(nbc,testTweet)

#######################################
# Bayesian Networks
# Hill-Climbing Algorithm
# Did not work
#######################################

# Flat Clustering Approaches

#K-Mediod clustering
install.packages("kmed")
library(kmed)
# With Numerical Variables
#tweet_nums<-m10_data[,c("retweet_count","favorite_count")]
#tweet_nums<-scale(tweet_nums)
#dist_m<-distNumeric(tweet_nums,tweet_nums)
#tweet_cluster<-fastkmed(dist_m,5)
#the_cluster<-tweet_cluster$cluster
#plot(tweet_nums[,1],tweet_nums[,2],col=the_cluster,xlim=c(0,1),ylim=c(0,1))

## Screen shot


#

#Hierarchical Clustering Approaches

#Bottom-Up:
#First, let's prepare the numerical data we would like to use:
tweet_nums<-m10_data[sample(1:nrow(m10_data),50),c("retweet_count","favorite_count")]
tweet_dist<-dist(tweet_nums)
fit<-hclust(tweet_dist)
plot(fit) # T Chart     ## *RUN
clust<-cutree(fit,k=2)
plot(tweet_nums$retweet_count,tweet_nums$favorite_count,col=clust) ## *RUN


