# Perform exploratory and KNN &/ cluster analysis on the attached dataset
# No domain knowledge is expected (unknown variables) 
# Focus on pure DM concepts and conceptual discoveries/insights
# ID all potential patterns /clusters-SHORT CLEAR comment on ALL discoveries
# See .csv file DMDATA_Q.csv attached

# DM Analytics:  
# attach(dmdata_Q)
# dm <- dmdata_Q
# dm <- read.csv(file="~/Desktop/dmdata_Q.csv",stringsAsFactors=F, na.strings=c("NA","NA"," NA","NA","NA"," NA","NA","NA"," NA","NA","NA"," NA"))
dm1 <- as.data.frame(sapply(dm, as.numeric))  #applying na.omit directly does not work
dm2 <- na.omit(dm1)      # done!

# verify dm2
str(dm2)   
dim(dm2);tail(dm2)

#use dm2 for further analysis and mining.

# Exploratory Analysis

# install.packages("DMwR")
# library(DMwR)

hist(dm2$e, prob=TRUE)
# Data is skewed more right (positive)
hist(dm2$x, prob=TRUE)
# Data is skewed more right (positive)
hist(dm2$a, prob=TRUE)
# Data is skewed more left (negative)
hist(dm2$y, prob=TRUE)
hist(dm2$f, prob=TRUE)
hist(dm2$y.1, prob=TRUE)
hist(dm2$z, prob=TRUE)
hist(dm2$b, prob=TRUE)
# Fairly normal distribution, where most values are around mean
hist(dm2$c, prob=TRUE)
# Fairly normal distribution, where most values are around mean
hist(dm2$d, prob=TRUE)
# Fairly normal distribution, where most values are around mean
hist(dm2$g, prob=TRUE)
# Fairly normal distribution, where most values are around mean

# install.packages("car")
# library(car)

# Identifying outliers
boxplot(dm2$e,ylab="e")
abline(h=mean(dm2$e,na.rn=T),lty=2)
  # Concentrated on low values, between 0 and 2


# KNN Analysis
library(class)
names(dm2)

data_norm <- function(x) { ((x - min(x))/ (max(x)- min(x)))}
dm2_norm <- as.data.frame(lapply(dm2[,-1], data_norm))
summary(dm2[,2:5]) # e: range from .000038 to 11.865, etc
summary(dm2_norm[,1:4]) # All between 0 and 1
dm2_train <- dm2_norm[1:450,]
dm2_test <- dm2_norm[451:3979,]

db2_pred <- knn(dm2_train, dm2_test, dm2[1:450,1], k=63)
table(db2_pred, dm2[451:3979,1])

# Cluster Analysis

# Data
plot(X ~ e, dm2)
plot(e ~ a, dm2)
plot(e ~ x, dm2) # most data towards 0 and then expands out
with(dm2,text(e~x, labels=X))

# Normalization
z <- dm2[,-c(1,1)]
z
m <- apply(z,2,mean)
s <- apply(z,2,sd)
z <- scale(z,m,s)

# Calculating Euclidean distance
distance <- dist(z)
distance
print(distance,digits=3)

# Cluster Dendrogram with Complete Linkage
hc.c <- hclust(distance)
plot(hc.c, labels=dm2$X)
plot(hc.c, hang = -1)

hc.a <- hclust(distance, method = "average")
plot(hc.a, hang=-1)

# Cluster Membership
member.c <- cutree(hc.c,3)
member.a <- cutree(hc.a,3)
table(member.c,member.a) 
# Using average linkage method: 
  # There are 15 + 11 observations linked to cluster 1
  # 188 + 1839 + 1916 observations linked to cluster 2
  # 10 observations linked to cluster 3

# Using the complete linkage method:
  # There are 15 + 188 observations that belong to cluster 1
  # 11 + 1839 observations that belong to cluster 2
  # 1916 + 10 observations that belong to cluster 3

# Comparing the two methods:
  # 15 was the best match for cluster 1,
  # 10 was the best match for cluster 3.
  # However, some mixed matched,
    # 188 has a membership in cluster 2 based on the average method, but has membership in cluster 1 based on complete method
# CLuster table allows to see how cluster formation is behaving
###
# Cluster Means
aggregate(z, list(member.c),mean) # Looks at variables signicance to clusters and their role in charaterizeing the clusters
# For example, larger differences, such as in y play a more significant role in deciding the cluster it is in
  # In particular, y's group 3 has higher numbers, while group 1 has much lower numebrs

aggregate(dm2[,-c(1,1)], list(member.c),mean) # Same, but with original units
###
library(cluster)
# Silhouette Plot # Load install. #######XXXXXXXXXX########XXXXXXXXXX
plot(silhouette(cutree(hc.c,3),distance))
###
# Scree Plot
wss <- (nrow(z)-1)*sum(apply(z,2,var))
for (i in 2:20) wss[i] <- sum(kmeans(z, center=i)$withinss)
plot(1:20, wss, type="b", xlab="Number of Clusters", ylab="Within group SS") # Overview of all possible clusters within group of sum of squares
  # Can see the first dop in SS is very large
  # From this we can conclude to go with smaller number of clusters

# K-Means Clustering - Identify centers
kc <- kmeans(z,3)
kc
# Cluster 1 contains: 927, 2: 1979, and 3: 1073
# Conclude that cluster 3 is closer together in terms of distance
# To see cluster memberships:
kc$cluster #Can do for all "Available Components"
kc$centers
plot(y~f, dm2, col = kc$cluster) # Colors represent cluster: Cluster 2 does not have much overlap while cluster 1 and 3 have some overlap

# Model Based Clustering
plot(dm2)
library(mclust)
fitM <- Mclust(dm2)
fitM
plot(fitM) #1. BIC, 2. classification, 3. uncertainty, 4. density

# Density Based clustering
install.packages("dbscan")
library(dbscan)
kNNdistplot(dm2, k = 3)
abline(h = 0.07, col = "red", lty = 2)
fitD <- dbscan(dm2, eps = 0.7, MinPts = 5)
fitD
plot(dm2, col = fitD)

