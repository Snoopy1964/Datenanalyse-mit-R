aa   <- c(1000,2000,3000)
mm   <- c(10,20,30)
sdsd <- c(10,10,10)

tt   <- 10*1:10



# sum plot, for smeared amounts (amount*norm)
# f.single <- function(td) {td[2]*dnorm(t[1], mean=[td[3]], sd=td[4])}
f.single <- function(x, a=1, m=0, sd=1) {  a*dnorm(x, mean=m, sd=sd)}

f.sum    <- function(x, aa, mm, sdsd) {
  df    <- data.frame(a=aa, m=mm, sd=sdsd)
  y.sum <- vector(mode='numeric', length=0)
  for(xt in x) {
    df$t  <- rep(xt, length(aa)) 
    y.sum <- c(y.sum, sum(transform(df, pA=f.single(t,a,m,sd))[, "pA"]))
  }
  return(y.sum)
}

testIt <- f.sum(tt, aa, mm, sdsd)

tt <- seq(as.numeric(as.Date("2012-07-01")),as.numeric(as.Date("2012-10-01")),0.1)
y <- f.sum(tt,SO.test[,"TotalPrice"], as.numeric(SO.test[,"predict.date"]),SO.test[,"predict.sd"])
tt <- as.Date(tt, origin="1970-01-01")

# plot sum per day
sum.day.pred <- aggregate(x=SO.test$TotalPrice/1000, by=list(SO.test$predict.date), FUN=sum)
names(sum.day.pred) <- c("predicted.day","TotalSum")
sum.day.pred$delivery.week <- as.numeric(format(sum.day.pred$predicted.day, "%W"))

plot(sum.day.pred[,1:2], type="b", lwd=2, 
     main="Vorhergesagte Einnahmen pro Tag",
     xlab="Kalenderwoche",
     ylab="Einnahmen in 1000€")

lines(as.Date(tt, origin="1970-01-01"),y/1000, lwd=2, col="blue")

# plot sum per week
sum.week.pred <- aggregate(x=sum.day.pred$TotalSum, by=list(sum.day.pred$delivery.week), FUN=sum)
names(sum.week.pred) <- c("predicted.week","TotalSum")

plot(sum.week.pred, type="b", lwd=2, xlim=c(26,39), 
     main="Vorhergesagte Einnahmen pro Woche",
     xlab="Kalenderwoche",
     ylab="Einnahmen in 1000€")

dens.sum  <- data.frame(t=tt, total.amount=y, t.week=as.numeric(format(tt, "%W")) )
dens.week <- aggregate(x=dens.sum$total.amount/1000*0.1, by=list(dens.sum$t.week), FUN=sum)
names(dens.week) <- c("week","TotalAmount")
lines(dens.week[,1:2], lwd=2, col="green")

# curve(f.sum(x,SO.test[,"TotalPrice"], as.numeric(SO.test[,"predict.date"]),SO.test[,"predict.sd"]), as.numeric(as.Date("2012-07-01")), as.numeric(as.Date("2012-10-01")))

# cumulated approach

sum.day <- aggregate(x=SO.test$TotalPrice, by=list(SO.test$predict.date), FUN=sum)
names(sum.day) <- c("predicted.date","TotalSum")
# plot(sum.day, type="l")
sum.day.lower <- aggregate(x=SO.test$TotalPrice, by=list(SO.test$predict.date.lower), FUN=sum)
names(sum.day.lower) <- c("predicted.date","TotalSumLower")
sum.day.upper <- aggregate(x=SO.test$TotalPrice, by=list(SO.test$predict.date.upper), FUN=sum)
names(sum.day.upper) <- c("predicted.date","TotalSumUpper")



cum <- vector("numeric")
for(i in 1:length(sum.day.pred$predicted.day)) {
  cum <- c(cum, sum(sum.day.pred$TotalSum[1:i]))
}
sum.day[1:3,]
sum.day.lower[1:3,]
sum.day.upper[1:3,]
plot(sum.day)
plot(sum.day, type="b")
lines(sum.day.lower, type="l", col="green")
lines(sum.day.upper, type="l", col="red")
sum.all <- merge(sum.day.lower, sum.day, all=TRUE)
sum.all <- merge(sum.all, sum.day.upper, all=TRUE)
sum.all
sum.all <- merge(sum.day.lower, sum.day)
sum.all
sum.day[1:3,]
sum.day.lower[1:3,]

som <- sum.day[1:3,]
sol <- sum.day.lower[1:3,]
sou <- sum.day.upper[1:3,]






