abalone <- read.csv("/Users/chirag/Desktop/Spring21/DA/Week5/abalone.csv", header = T)
View(abalone)
str(abalone)
summary(abalone$Rings)


# For now, we’ll break the rings variable
# into 3 levels" “young” for abalones less than 8, “adult” for abalones between 8-11,
# and “old” for abalones older than 11.
(abalone$Rings <- as.numeric(abalone$Rings))
(abalone$Rings <- cut(abalone$Rings, br=c(-1,8,11,35),labels = c("young", 'adult', 'old')))
(abalone$Rings <- as.factor(abalone$Rings))
summary(abalone$Rings)


# remove the "sex" variable in abalone coz KNN requires all numeric variables for prediction
aba <- abalone
aba$Sex <- NULL
View(aba)


# normalize the data using min max normalization
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

aba[1:7] <- as.data.frame(lapply(aba[1:7], normalize))
summary(aba$Shucked.weight)


# After Normalization, each variable has a min of 0 and a max of 1.
# in other words, values are in the range from 0 to 1.
# We’ll now split the data into training and testing sets.
ind <- sample(2, nrow(aba), replace=TRUE, prob=c(0.7, 0.3))
KNNtrain <- aba[ind==1,]
KNNtest <- aba[ind==2,]
sqrt(2918)


# make k equal to the square root of 2918, the number of observations in the training set.
# sqrt(2918) ~= 54.01852 round it to 55 and use k = 55 # We usually take an Odd number for k value,
# knn model
# knn() is in the "class" library. Make sure to install it first on your RStudio.
library(class)
help("knn") # Read the knn documentation on RStudio.
(KNNpred <- knn(train = KNNtrain[1:7], test = KNNtest[1:7], cl = KNNtrain$Rings, k = 55))
table(KNNpred)


