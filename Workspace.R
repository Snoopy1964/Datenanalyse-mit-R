today <- "2012-07-02"
str(today)
d1 <- as.Date(today, format="%Y-%m-%d")
str(d1)

SO <- SalesOrders[1:10,]

for(i in 1:length(SO$DeliveryDate)) {
  SO$DeliveryDate[i] <- as.Date(SO$DeliveryDate[i], format="%Y-%m-%d")
}

DD <- c(Sys.Date())
for(i in 1:length(SO$DeliveryDate)) {
  DD[i] <- as.Date(SO$DeliveryDate[i], format="%Y-%m-%d")
}

attach(SalesOrders)
opar <- par(mfrow=c(2,2))
hist(BuyerRef)
hist(DeliveryDate, "days")
hist(TotalPrice)
hist(PaymentDuration)

cls10    <- kmeans(PaymentDuration, centers=3, iter.max=10)
cls25    <- kmeans(PaymentDuration, centers=3, iter.max=25)
cls50    <- kmeans(PaymentDuration, centers=3, iter.max=50)
cls100   <- kmeans(PaymentDuration, centers=3, iter.max=100)
cls250   <- kmeans(PaymentDuration, centers=3, iter.max=250)
cls500   <- kmeans(PaymentDuration, centers=3, iter.max=500)
cls1000  <- kmeans(PaymentDuration, centers=3, iter.max=1000)
cls2500  <- kmeans(PaymentDuration, centers=3, iter.max=2500)
cls5000  <- kmeans(PaymentDuration, centers=3, iter.max=5000)
cls10000 <- kmeans(PaymentDuration, centers=3, iter.max=10000)

nr.cl <- c(10, 25, 50, 100, 250, 500, 1000, 2500, 5000, 10000)
cls   <- c(cls10$centers, cls25$centers, cls50$centers, cls100$centers, cls250$centers, cls500$centers, cls1000$centers, cls2500$centers, cls5000$centers, cls10000$centers)

# SOClustered <- cbind(SalesOrders, customer.group=cls$cluster)
SO.cg1 <- SalesOrders[cls$cluster==1,]
SO.cg2 <- SalesOrders[cls$cluster==2,]
SO.cg3 <- SalesOrders[cls$cluster==3,]
length(SO.cg1$BuyerRef)
length(SO.cg2$BuyerRef)
length(SO.cg3$BuyerRef)


# plot.Mclust
clPairs(Pd[,2:4], classification = cl.m$classification)
coordProj(data = Pd[,2:4], parameters = cl.m$parameters, 
          z = cl.m$z, what = "classification", 
          identify = TRUE)
coordProj(data = Pd[,2:4], parameters = cl.m$parameters, 
          z = cl.m$z, what = "uncertainty", dimens = c(2,3), 
          identify = TRUE)

# compare cluster algorithms
cl.km <- kmeans(Pd[,2:4],3)
cl.m  <- Mclust(Pd[,2:4])
t(cl.km$centers)[,c(1,3,2)]
cl.m$parameters$mean
plot(Pd[,2:4], col=cl.km$cluster, pch=cl.km$cluster)
plot(Pd[,2:4], col=cl.m$classification, pch=cl.m$classification)

# sum plot, (min,max) with single amount Salesorders
sum.week.pred         <- aggregate(x=SO.test$TotalPrice/1000, by=list(SO.test$predict.week), FUN=sum)
names(sum.week.pred)  <- c("predicted.week","TotalSum")
sum.week.lower        <- aggregate(x=SO.test$TotalPrice/1000, by=list(SO.test$predict.week.lower), FUN=sum)
names(sum.week.lower) <- c("predicted.week","TotalSum.lower")
sum.week.upper        <- aggregate(x=SO.test$TotalPrice/1000, by=list(SO.test$predict.week.upper), FUN=sum)
names(sum.week.upper) <- c("predicted.week","TotalSum.upper")

sum.week.pred <- merge(sum.week.pred, sum.week.lower, all=TRUE)
sum.week.pred <- merge(sum.week.pred, sum.week.upper, all=TRUE)

plot(sum.week.pred[,c(1,2)], type="b", lwd=2, xlim=c(26,39),
     main="Vorhergesagte Einnahmen pro Woche",
     xlab="Kalenderwoche",
     ylab="Einnahmen in 1000€")
lines(sum.week.pred[, c(1,3)], type="l", col="blue")
lines(sum.week.pred[, c(1,4)], type="l", col="red")

# sum plot, for smeared amounts (amount*norm)
# f.single <- function(td) {td[2]*dnorm(t[1], mean=[td[3]], sd=td[4])}
f.single <- function(x, a=1, m=0, sd=1) {  a*dnorm(x, mean=m, sd=sd)}

f.sum    <- function(x, aa, mm, sdsd) {
  
  df    <- data.frame(t=rep(t,  length(aa)),a=aa, m=mm, sd=sdsd)
  a.sum <- transform(df, pA=f.single(t,a,m,sd))[, "pA"]
}
