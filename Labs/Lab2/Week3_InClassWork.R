library("xlsx")
filename = '/Users/chirag/Desktop/Spring21/DA/Week3/multivariate.xls'
(mv <- read.xlsx(filename, 2, header=TRUE))

attach(mv)
names(mv)

plot(Income, Immigrants, main = 'Scatterplot for Income of Immigrats')
plot(Immigrants, Homeowners)

(mm <- lm(Homeowners ~ Immigrants))
abline(mm, col = 2, lwd = 3)
summary(mm)
attributes(mm)
mm$coefficients

HP <- Homeowners / Population
PD <- Population / area
(mm <- lm(Immigrants ~ Income + Population + HP + PD))
summary(mm)
(cm <- coef(mm))
