# Clustering & Graphical Methods

# Model with lower BIC is preferred

#A Helper Function that will convert the tibble into a csv format
df_unlist<-function(df){
  newdf = data.frame(temp = rep(0,nrow(df)),stringsAsFactors = FALSE)
  for( i in colnames(df)){
    compVec = c()
    if(is.list(df[,i])){
      for(j in 1:nrow(df)){
        t = paste(unlist(df[j,i]),collapse = ";",sep="")
        compVec = c(compVec,t)
      }
    }else{
      compVec = df[,i]
    }
    newdf=data.frame(newdf,compVec)
  }
  newdf = newdf[,-1]
  colnames(newdf) = colnames(df)
  return(newdf)
}

#Load data
tweets <- read.csv("tweets.csv")

######## Flat Clustering Approaches ##########

# K-Mediod clustering
install.packages("kmed")
library(kmed)

# With numerical variables
tweet_nums<-tweets[,c("retweet_count","favorite_count")]
tweet_nums<-scale(tweet_nums)
dist_m<-distNumeric(tweet_nums, tweet_nums) # distNumeric: comes from kmed package
tweet_cluster<-fastkmed(dist_m,5)
the_cluster<-tweet_cluster$cluster
plot(tweet_nums[,1],tweet_nums[,2],col=the_cluster,xlim=c(0,1),ylim=c(0,1))

# to check NA's
which(is.na(tweet_nums))

# With Categorical Variables
# Exercise
tweet_cat<-as.matrix(tweets[,c("is_quote","verified")])
tweet_dist<-matching(tweet_cat,tweet_cat)
cooccur(tweet_cat)

#######################################################

######## Hierarchical Clustering Approaches ###########

# Bottom-Up
# First, let's prepare the numerical data we would like to use:
tweet_nums<-tweets[sample(1:nrow(tweets),50),c("retweet_count","favorite_count")]
tweet_dist<-dist(tweet_nums)
fit<-hclust(tweet_dist)
plot(fit)
clust<-cutree(fit,k=5)
plot(tweet_nums$retweet_count,tweet_nums$favorite_count,col=clust) #,xlim=c(0,100),ylim = c(0,500))

#1,286.54
#950.45
#336.09

#######################################################
