# Bayesian_Networks

install.packages("bnlearn")
library(bnlearn)
source("http://bioconductor.org/biocLite.R")
biocLite(c("RBGL"))

# Load in test data:
data(learning.test)

# Learn the structure of the Bayesian Network
# Using the Hill-Climbing Algorithm
bn.graph<-hc(learning.test)
plot(bn.graph)

# Now we have our structure, and we have our data.
# Last step: find the conditional probability distributions!
fitted<-bn.fit(bn.graph,learning.test)

# Now that we have learned the model, let us use it for inference (Approx)
c_dist<-cpdist(fitted,nodes = c("E"),evidence = TRUE,n=100000)
table(c_dist)/nrow(c_dist)

# Now suppose we knew something about "A": ## Finding conditional probabilities
c_dist<-cpdist(fitted,nodes=c("E"),evidence = (A=="a")&(F=="a"),n=100000)
table(c_dist)/nrow(c_dist)

# Now let's try to have some fun with stock data !
install.packages("quantmod")
library(quantmod)
{
#Download the stocks:
s1<-data.frame(getSymbols("AAPL",auto.assign=FALSE))
s2<-data.frame(getSymbols("MSFT",auto.assign=FALSE))
s3<-data.frame(getSymbols("CSCO",auto.assign=FALSE))

#Compute the daily change from previous close to open:
s1_c<-s1[-1,]
s2_c<-s2[-1,]
s3_c<-s3[-1,]

s1_b<-s1[-nrow(s1),]
s2_b<-s2[-nrow(s2),]
s3_b<-s3[-nrow(s3),]

s1_diff<-s1_c$AAPL.Open-s1_b$AAPL.Close
s2_diff<-s2_c$MSFT.Open-s2_b$MSFT.Close
s3_diff<-s3_c$CSCO.Open-s3_b$CSCO.Close

#Now, replace all negative numbers with the market "d", and all positive/zero with "u":
s_1<-rep("u",length(s1_diff))
s_1[which(s1_diff<=0)]<-"d"

s_2<-rep("u",length(s2_diff))
s_2[which(s2_diff<=0)]<-"d"

s_3<-rep("u",length(s3_diff))
s_3[which(s3_diff<=0)]<-"d"
}

stock.data<-data.frame(AAPL=s_1,MSFT=s_2,CSCO=s_3)

# Now learn the structure of the data:
bn.graph<-hc(stock.data)
plot(bn.graph)
fitted<-bn.fit(bn.graph,stock.data)

c_dist<-cpdist(fitted,nodes = c("CSCO"),evidence = TRUE,n=100000)
table(c_dist)/nrow(c_dist)

c_dist<-cpdist(fitted,nodes = c("CSCO"),evidence = (MSFT=="d")&(AAPL=="d"),n=100000)
table(c_dist)/nrow(c_dist)

