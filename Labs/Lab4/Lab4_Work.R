set.seed(12345)
help(par)
# par can be used to set or query graphical parameters.
# Parameters can be set by specifying them as arguments 
# to par in tag = value form, or by passing them as a list of tagged values.
(par(mar = rep(0.2,4)))
data_Matrix <-matrix(rnorm(400), nrow = 40)
image(1:10, 1:40, t(data_Matrix)[,nrow(data_Matrix):1])
help("heatmap")
help(rep)
heatmap(data_Matrix)


set.seed(678910)
for(i in 1:40){
  # flipping a coin and getting the data
  coin_Flip <- rbinom(1, size = 1, prob = 0.5)
  # if the coin is "Heads", add a common pattern to that row,
  if(coin_Flip){
    data_Matrix[i, ] <- data_Matrix[i, ] + rep(c(0,3), each =5)
  }
}
(par(mar = rep(0.2,4)))
image(1:10, 1:40, t(data_Matrix)[,nrow(data_Matrix):1])
heatmap(data_Matrix)


hh <- hclust(dist(data_Matrix))
data_Matrix_ordered <- data_Matrix[hh$order, ]
par(mfrow = c(1,3))
image(t(data_Matrix_ordered)[, nrow(data_Matrix_ordered):1])
plot(rowMeans(data_Matrix_ordered), 40:1, xlab = "The Row Mean", ylab = "Row", pch = 19)
plot(colMeans(data_Matrix_ordered), xlab = "The Column Mean", ylab = "Column", pch = 19)


install.packages("gdata")
library(gdata) 
#faster xls reader but requires perl!
bronx1<-read.xls(file.choose(),pattern="BOROUGH",stringsAsFactors=FALSE,sheet=1,perl="<SOMEWHERE>/perl/bin/perl.exe") 
bronx1<-bronx1[which(bronx1$GROSS.SQUARE.FEET!="0" & bronx1$LAND.SQUARE.FEET!="0" & bronx1$SALE.PRICE!="$0"),]