# load some package for a nice visualization
library("car")
#library("sm")
library("mclust")


# load data from csv file
SalesOrders <- read.csv("data/SalesOrders.csv", 
                        sep=";",
                        stringsAsFactors=FALSE)
# convert strings to dates
SalesOrders$DeliveryDate <- as.Date(SalesOrders$DeliveryDate, format="%Y-%m-%d")

# Visualize raw data
scatterplotMatrix(SalesOrders, cex=0.6)

# Prepare for customer specific analysis
# Time Series Objects
Pd.ts     <- aggregate(x=SalesOrders$PaymentDuration, by=list(Customer=SalesOrders$BuyerRef), FUN=ts)
names(Pd.ts) <- c("Customer", "TimeSeries")

# Mean value of time series
Pd.mean        <- sapply(Pd.ts[, "TimeSeries"], FUN=mean)
names(Pd.mean) <- c("Mean")
# Standard deviation of time series
# for R "runaways" :-)
Pd <- cbind(Pd.ts, Mean=Pd.mean, StdDev=sapply(Pd.ts[, "TimeSeries"], FUN=sd))

# example for more attributes and merge
Pd.nr          <- aggregate(x=SalesOrders$PaymentDuration, by=list(Customer=SalesOrders$BuyerRef), FUN=length)
names(Pd.nr)   <- c("Customer", "NrDel")
Pd <- merge(Pd, Pd.nr)

# sort the columns (Attribute "TimeSeries" at the end -> printout looks much nicer)
Pd <- Pd[, c(1,3,4,5,2)]

# define predictor
predict.payDur <- function(ID) {
    i <- match(ID, Pd$Customer, nomatch = 0)
    if(i==0) {
      # unknown customer, best guess:
      # return mean value and standard deviation of all historical PaymentDuration
      payDur.mean <- mean(unlist(Pd[,"TimeSeries"]))
      payDur.sd   <- sd(unlist(Pd[,"TimeSeries"]))
    } else {
      payDur.mean <- Pd[Pd$Customer == Pd$Customer[i], "Mean"]
      payDur.sd   <- Pd[Pd$Customer == Pd$Customer[i], "StdDev"]
    }
    return(c(payDur.mean, payDur.sd))
}


# create some sample SalesOrders for testing
set.seed(1234)               # set seed for random number generation
d0         <- as.Date("1970-01-01")  # set constant begin of date
date.min   <- as.Date("2012-07-01")
date.max   <- as.Date("2012-08-31")
nr         <- 1000
cust.test  <- round(runif(nr, min=1001, max=1100))
date.test  <- d0 + round(runif(nr, min = date.min-d0, max = date.max-d0))
total.test <- 10000+round(rnorm(nr, mean=10000, sd=5000))

SO.test    <- data.frame(Customer=cust.test, 
                         DeliveryDate=date.test, 
                         delivery.week=as.numeric(format(date.test, "%W")),
                         TotalPrice=total.test
)

# estimation of sum per week
sum.week <- aggregate(x=SO.test$TotalPrice/1000, by=list(SO.test$delivery.week), FUN=sum)
names(sum.week) <- c("delivery.week","TotalSum")

sum.week$target.week <- sum.week$delivery.week+3

# plot(sum.week[, c("delivery.week", "TotalSum")], type="b", col="blue")
# lines(sum.week[, c("target.week", "TotalSum")], type="b", col="red")

sum.week$target.week <- sum.week$delivery.week+3


# apply predictor
cust.pred <- t(sapply(SO.test$Customer, FUN=predict.payDur))
SO.test   <- cbind(SO.test,
                   predict.mean=cust.pred[,1], 
                   predict.lower=cust.pred[,1]-cust.pred[,2],
                   predict.upper=cust.pred[,1]+cust.pred[,2],
                   predict.sd=cust.pred[,2],
                   predict.date=SO.test$DeliveryDate       + round(cust.pred[,1]),
                   predict.date.lower=SO.test$DeliveryDate + round(cust.pred[,1]-cust.pred[,2]),
                   predict.date.upper=SO.test$DeliveryDate + round(cust.pred[,1]+cust.pred[,2])
)
SO.test$predict.week       <- as.numeric(format(SO.test[,9], "%W"))
SO.test$predict.week.lower <- as.numeric(format(SO.test[,10], "%W"))
SO.test$predict.week.upper <- as.numeric(format(SO.test[,11], "%W"))

# plot sum per day
# sum.day <- aggregate(x=SO.test$TotalPrice, by=list(SO.test$predict.date), FUN=sum)
# names(sum.day) <- c("predicted.date","TotalSum")
# plot(sum.day, type="l")

# plot sum per week
sum.week.pred <- aggregate(x=SO.test$TotalPrice/1000, by=list(SO.test$predict.week), FUN=sum)
names(sum.week.pred) <- c("predicted.week","TotalSum")

plot(sum.week.pred, type="b", lwd=2, xlim=c(26,39), 
     main="Vorhergesagte Einnahmen pro Woche",
     xlab="Kalenderwoche",
     ylab="Einnahmen in 1000€")
lines(sum.week[, c("delivery.week", "TotalSum")], type="l", col="blue")
lines(sum.week[, c("target.week", "TotalSum")], type="l", col="red")

# define function for moving average
moving.average <- function(ts.data, weights=rep(1,times=5)){
  # set filter for moving average
  k <- weights
  k <- k/sum(k)
  if(length(ts.data) > length(k)) {
    return(filter(ts.data, method="convolution", sides=1, k))
  } else {
    return(ts.data)
  }
}

Pd.fts <- lapply(Pd[,5], FUN=moving.average)



