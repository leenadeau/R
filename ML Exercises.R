fx <- function(x){x^2}
fx(3)

## Exercise 1

# How many Data Scientists
# Enter the data
g <- c(3, 5, 0, 0, 2, 4, 2, 3, 7, 4, 5)
n <- length(g)
ct <- 0

# Create a function
findg <- function(g,n,ct){
  for (i in 1:n)
  {
    ifelse( g[i] <= ct, (ct=ct + 1), ct) # self-updating / LEARNING "ct"
  }
  return(ct)
}

findg(g,n,ct)


## Exercise 2

# How many Data Scientists
# Enter the data
g <- c(3, 5, 0, 0, 2, 4, 2, 3, 7, 4, 5, 3, 5, 0, 8, 2, 4, 2, 3, 7, 4, 5, 3, 5, 0, 19, 2, 4, 2, 3, 7, 4, 5, 3, 5, 20, 30, 2, 4, 2, 3, 17 , 4, 5, 3, 5, 0, 30, 2, 4, 2, 3, 7, 14, 15)
n <- length(g)
ct <- 0

# Create a function
findg <- function(g,n,ct){
  for (i in 1:n)
  {
    ifelse( g[i] <= ct, (ct=ct + 1), ct) # self-updating / LEARNING "ct"
  }
  return(ct)
}

findg(g,n,ct)


## Exercise 3

# Create a list of requirements for 1000 guests: N = 1000
# Each of the guests requires between 0 & 70    Create g ~ {0:70}
# Find total registrations

g1 <- sample(0:70, 1000, replace = T)
n <- length(g1)
ct <- 0
findg(g1,n,ct)


## finally
### create a plot for all value of ct to see the progression of "learning"
findgx <- function(g,n,ct,x){
  for (i in 1:n)
  {
    ifelse( g[i] <= ct, (ct=ct + 1), ct) # self-updating / LEARNING "ct"
    x[i] <- ct
  }
  return(x)
}
####
g1 <- sample(0:70, 1000, replace = T)
n1 <- length(g1)
ct <- 0
x <- rep(0,n1)

findgx(g1,n1,ct,x)
y <- findgx(g1,n1,ct,x)
y
plot(y,asp=1)
##############################################################
# Figure out solution to create animations to solve rubiks cube.

##############################################################


# SVM Tutorial
# Attach Packages
install.packages("tidyverse")
library("tidyverse")            # Data manipulation and visualization
install.packages("kernlab")
library("kernlab")              # SVM Methodology
install.packages("e1071")
library("e1071")                # SVM Methodology 
install.packages("ISLR")
library("ISLR")                 # Contains example data set "khan"
install.packages("RColorBewer")
library("RColorBewer")          # Customized coloring of plots
library(ggplot2)
# Set seed
set.seed(10)

# Construct sample data set - linear separable
x <- matrix(rnorm(20*2), ncol = 2)
y <- c(rep(-1,10), rep(1,10))
x[y==1,] <- x[y==1,] + 3/2
dat <- data.frame(x=x, y=as.factor(y))

# Plot data
ggplot(data = dat, aes(x = x.2, y=x.1, color=y, shape=y)) +
  geom_point(size = 2) +
  scale_color_manual(values = c("#000000", "#FF0000")) +
  theme(legend.position = "none")

# e1071 Fit SUpport Vector Machine model to data set
svmfit <- svm(y~., data = dat, kernel = "linear", scale = FALSE)
# Plot Results
plot(svmfit, dat)

# Sample plot, diff 
kernfit <- ksvm(x, y, type = "C-svc", kernel = 'vanilladot')
plot(kernfit, data = x)

## SVM MAchine learning applid to a sample data set ^^^^^^^^^^^

##########################
############################
# New data - mixes ============================================
# Construct sample data set ~not completely separated
x <- matrix(rnorm(20*2), ncol = 2)
y <- c(rep(-1,10), rep(1,10))
x[y==1,] <- x[y==1,] + 1
dat <- data.frame(x=x, y=as.factor(y))

# Plot data
ggplot(data = dat, aes(x = x.2, y=x.1, color=y, shape=y)) +
  geom_point(size = 2) +
  scale_color_manual(values = c("#000000", "#FF0000")) +
  theme(legend.position = "none")

# Fit Support Vector Machine model to data set
svmfit <- svm(y~., data = dat, kernel = "linear", cost = 3)
# Plot Results
plot(svmfit, dat)

# Fit Support Vector Machine model to data set
svmfit <- svm(y~., data = dat, kernel = "linear", cost = 0.1) # Change c
# Plot Results
plot(svmfit, dat)

# Fit Support Vector Machine model to data set
svmfit <- svm(y~., data = dat, kernel = "linear", cost = 7) # Change c
# Plot Results
plot(svmfit, dat)

# Fit Support Vector Machine model to data set
svmfit <- svm(y~., data = dat, kernel = "linear", cost = 22)  # Change c
# Plot Results
plot(svmfit, dat)

# Fit Support Vector Machine model to data set
kernfit <- ksvm(x, y, type = "C-svc", kernel = 'vanilladot', C = 100)
# Plot Results
plot(kernfit, data = x)

# Find optimal cost of misclassification
tune.out <- tune(svm, y~., data = dat, kernel = "linear",
                 ranges = list(cost = c(0.001, 0.01, 0.1, 1, 5, 10, 100)))
# Extract the best model
bestmod <- tune.out$best.model
bestmod
##

# Create a table of misclassified observations 
ypred <- predict(bestmod, dat)
misclass <- table(predict = ypred, truth = dat$y)
misclass

#########################################################
# Construct larger random data set
x <- matrix(rnorm(200*2), ncol = 2)
x[1:100,] <- x[1:100,] + 3.5
x[101:150,] <- x[101:150,] - 3.5
y <- c(rep(1,150), rep(2,50))
dat <- data.frame(x=x,y=as.factor(y))

# Plot data
ggplot(data = dat,aes(x = x.2, y = x.1, color = y, shape = y)) +
  geom_point(size = 2) +
  scale_color_manual(values = c("#000000","red")) +
  theme(legend.position = "none")

# Set pseudoraNDOM number generator
set.seed(56)
# Sample training data and fit model
train <- base::sample(200,100, replace = FALSE)   # Create train numbers
svmfit <- svm(y~., data = dat[train,], kernel = "radial", gamma = 1, cost = 1) # apply train number
# Plot classifier
plot(svmfit, dat)

# Fit radial-based SVM in kernlab
kernfit <- ksvm(x[train,], y[train], type = "C-svc", kernel = 'rbfdot', c = 1, scaled = c())
# Plot training data
plot(kernfit, data = x[train,])

# Tune model to find optimal cost, gamma values
tune.out <- tune(svm, y~., data = dat[train,], kernel = "radial",
                 ranges = list(cost=c(0.1,1,10,100,1000),
                               gamma = c(0.5,1,2,3,4)))
# Show best model
tune.out$best.model

# Valid model performance
(valid <- table(true = dat[-train,"y"], pred = predict(tune.out$best.model,
                                                       newx = dat[-train,])))

###############################################################################
# SVM for multiple classes
# Construct data set
x <- rbind(x, matrix(rnorm(50*2), ncol = 2))
y <- c(y, rep(0,50))
x[y==0,2] <- x[y==0,2] + 2.5
dat <- data.frame(x=x, y=as.factor(y))
# Plot data set
ggplot(data = dat, aes(x = x.2, y = x.1, color = y, shape = y)) +
  geom_point(size = 2) +
  scale_color_manual(values = c("000000", "#FF0000", "#00BA00")) +
  theme(legend.position = "none")

# Fit model
svmfit <- svm(y~., data = dat, kernel = "radial", cost = 10, gamma = 1)
# Plot results

# Construct table
ypred <- predict(svmfit, dat)
(misclass <- table(predict = ypred, truth = dat$y))






dat <- data.frame(x = khan$xtrain, y=as.factor(khan$ytrain))
out <- svm(y~., data = dat, kernel = "linear", cost = 10)
out

table(out$fitted, dat$y)
