library(ISLR) 
library(MASS)
library(boot) 
set.seed(1)

train = sample(392,196)
lm.fit <- lm(mpg~horsepower, data = Auto, subset = train)
mean((Auto$mpg-predict(lm.fit,Auto))[-train]^2)

lm.fit2 <- lm(mpg~poly(horsepower,2), data = Auto, subset = train) # Quadratic  
mean((Auto$mpg-predict(lm.fit2,Auto))[-train]^2)

lm.fit3 <- lm(mpg~poly(horsepower,3), data = Auto, subset = train) # Cubic  
mean((Auto$mpg-predict(lm.fit3,Auto))[-train]^2)

set.seed(2)
train = sample(392,196)
lm.fit <- lm(mpg~horsepower, data = Auto, subset = train)  
mean((Auto$mpg-predict(lm.fit,Auto))[-train]^2)
lm.fit2 <- lm(mpg~poly(horsepower,2), data = Auto, subset = train) # Quadratic  
mean((Auto$mpg-predict(lm.fit2,Auto))[-train]^2)
lm.fit3 <- lm(mpg~poly(horsepower,3), data = Auto, subset = train) # Cubic  
mean((Auto$mpg-predict(lm.fit3,Auto))[-train]^2)

install.packages('randomForest')
library(randomForest)
data1 <- read.csv(file.choose(), header = T)
head(data1)
colnames(data1) <- c("BuyingPrice","Maintenance","NumDoors","NumPersons","BootSpace","Safety","Condition")
head(data1)
str(data1)
levels(data1$Condition)
summary((data1))

set.seed(100)
train <- sample(nrow(data1),0.7*nrow(data1), replace = F)
TrainSet <- data1[train, ]
ValidSet <- data1[-train, ]
model1 <- randomForest(Condition ~ ., data = TrainSet, importance = T)
