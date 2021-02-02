#Committed in Test Branch

# EPI Dataset Analysis
epi_data = read.csv('/Users/chirag/Desktop/Spring21/DA/EPI_data.csv', header = T)
View(epi_data)
head(epi_data)
dim(epi_data)
names(epi_data)
str(epi_data)
summary(epi_data)
summary(epi_data$Population07)
boxplot(epi_data$Population07, xlab = 'Population', main = 'Extreme Populations')


# Boston Dataset Analysis
install.packages("MASS")
library(MASS)
?Boston
nrow(Boston)
ncol(Boston)
head(Boston)
dim(Boston)
names(Boston)
str(Boston)
summary(Boston)
summary(Boston$crim)
hist(Boston$crim, xlab = 'Per Capita Crime', main = 'Crime in Boston')
boxplot(Boston)
boxplot(Boston$indus, main = 'Industrial Area')


# Auto Dataset Analysis
install.packages("ISLR")
library(ISLR)
head(Auto)
names(Auto)
fivenum(Auto$mpg)
fivenum(Auto$weight)
boxplot(Auto$weight)
mean(Auto$weight)
median((Auto$weight))