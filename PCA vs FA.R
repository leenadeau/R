# PCA vs FA
# Principal Component Analysis Vs. Factor Analysis

library(quantmod)
library(psych)


###############################################Principal Component Analysis#####################
#We are going to try and determine if we can "group" stock prices together for different stocks.
#This is one approach to create a stock index.  We will assume that we know nothing about the
#number of "indexes", and only pull from the 7 stocks that are below.  I have selectively chosen
#three groups of stocks.  Three "Communications" stocks, two, "Consumer Discretionary" stocks, and
#two "Financial" stocks.  The plan: let PCA (1) tell us how many indexes we should create out
#of these stocks, and (2) how should we use the raw stock data for these 7 stocks to compute 
#the values of the indices.  As mentioned, PCA should return to us 3 indexes.

stocks<-c("DISH","CMCSA","HD","LOW","JPM","MS")
getSymbols(stocks)

#Put the closing stock price (column 4) for all stocks in the same data frame: 
stock_prices<-rep(0,nrow(get(stocks[1])))

for(x in stocks){
  stock_prices<-cbind(stock_prices,as.numeric(get(x)[,4]))
}

stock_prices<-data.frame(stock_prices[,-1])
names(stock_prices)<-stocks

#Save the latest 5 observations for testing purposes:
test.prices<-stock_prices[(nrow(stock_prices)-4):nrow(stock_prices),]
stock_prices<-stock_prices[-c((nrow(stock_prices)-4):nrow(stock_prices)),]
#Notice the correlation matrix below.  We see than many of our variables are highly correlated.
#In the real world, a correlation, even as low as .1, is considered "high", depending on the
#context.  We notice that all of our correlations are very high.  This will most certainly cause
#problems if we were conducting a supervised learning task, such as regression, using these
#variables.  Hence, a reduction in the dimensions is necessary.  In this case, we want to map
#each stock onto a new "variable", one that we are not able to observe directly, but can compute
#a value for based on our observed variables.  Not only that, but we want to find new and smaller
#variables that themselves will not be highly correlated.  We can do this using the PCA approach.

cor(stock_prices)

#Okay, first, we want to determine how many components we should use.  Again, I will reiterate,
#I hand picked different stocks from 3 different sectors of the market.  Let us see if PCA can
#concur with reality.  

#First, run a PCA on a large number of components.  We use the "scale. = T" optinon to scale all
#of the data (that is, normalize it to a normal distribution of mean 0 and standard deviation 1.)
#In other words, it compute a z-score for each data point and replaces each data-point with it's 
#z-score.
prin_comp <- prcomp(stock_prices, scale. = T)
#Now, compute the percentage of variance in the data that was captured from every component:
std_dev <- prin_comp$sdev
pr_var <- std_dev^2
prop_varex <- pr_var/sum(pr_var)

#Let's plot the amount of variance explained against the number of components:
plot(prop_varex,type="b")
plot(cumsum(prop_varex),type="b",xlab="Number of Components",ylab="Cumulative Variance Explained")

#We see from the previous two plots that 3 components will explain over 98% of the variation in the data.
#A standard rule of thumb is to use the number of components that explain up to 98%.  Any additional components 
#will not add much value in explaination.  Hence, we will re-run the PCA with only three components:
prin_comp_final <- prcomp(stock_prices, scale. = T, rank.=3)

#We have sucessfully converted our 7 variables into 3 variables.  We can check the correlations between
#our three new variables.  The "x" portion of the PCA object returned to us is the collection of values
#for the three new variables (i.e. components).  This is simply the weight average of the original scaled
#stock prices and the weights (the weights are found in the "rotation" portion of the PCA object returned to us).
#Notice that the correlations between the components are very low.  This is good news, and if we were conducting
#a regression of some kind, we would now have uncorrelated independent variables (i.e. the components) to use.
cor(prin_comp_final$x)

#If we are given a new observation for all of the stocks, we can use the predict function to compute the 
#corresponding component scores for each component:
predict(prin_comp_final,test.prices)
###########################################################################################################################


###############################################Factor Analysis###############################################
#We are going to use scale items from the results of an iq test to illustrate factor analysis.
#library(psych)

data(iqitems)

#The process is simliar to PCA.  First, we find a scree plot to determine the optimal number of factors:
fa.parallel(iqitems,fm="minres")

#We see diminishing returns after 4 factors.  Hence, we will use 4 factors. Now, run the factor analysis:
fa_iq<-fa(iqitems,fm="minres",rotate="varimax",nfactors=4)

#Print the factor loadings.  The general rule of thumb is that if the loading is less than .3, we 
#drop the loading:
print(fa_iq$loadings,cut=.3)

fa.diagram(fa_iq)


#############################################################################################################
