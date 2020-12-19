# Get the Data
library(ISLR)

# Check the head of the iris Data  Frame
str(iris)
head(iris)

# Standardized Data
# the iris data set has all its features in the same order of magnitude
# ,but its good practice (expecially with KNN) to standardize features 
# in your data

standardized.iris <- scale(iris[1:4])

var(standardized.iris[,1])

#summary(standardized.iris)

final.iris <- cbind(standardized.iris,iris[5])

#TRAIN AND TEST SPLITS

library(caTools)
set.seed(101)

sample <- sample.split(final.iris$Species, SplitRatio = 0.70)
data.train <- subset(final.iris, sample == TRUE)
data.test <- subset(final.iris, sample == FALSE)

#Build a KNN model 
# use the knn function to predict Species of the test set. Use k = 1
library(class)

predicted.species <- knn(data.train[1:4],data.test[1:4],data.train$Species,k=1)
predicted.species



missClassierror <- mean(data.test$Species != predicted.species)
print(missClassierror)
# missClassierror = 0.04444444



##### 
# Choosing K values

predicted.species <- NULL
erate <- NULL

for (i in 1:20){
  set.seed(101)
  predicted.species <- knn(data.train[1:4],data.test[1:4],data.train$Species,k=i)
  erate[i] <- mean(data.test$Species != predicted.species)
}
print(erate)

# error drops to its lowest for k values between 2-6. 

library(ggplot2)
k.values <- 1:20
error.df <- data.frame(k.values,erate) 

ggplot(error.df,aes(k.values,erate)) + geom_point() + geom_line(lty = 'dotted', color = 'red')
