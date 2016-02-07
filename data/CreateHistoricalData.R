d0 <- as.Date("1970-01-01")
set.seed(1111)
nr          <- 1000
# IDs         <- 1:nr
date.min    <- as.Date("2012-01-01")
date.max    <- as.Date("2012-06-30")
# dates       <- d0 + round(runif(nr, min = date.min-d0, max = date.max-d0))
# dates       <- date.min + round(rpois(nr, lambda=28))
# dates       <- date.min + round(rexp(nr, rate=0.05))
# prices      <- round(runif(nr)*10000+18000)

# Customers Germany -> payment target 21 days, 3% cash discount within 7 days
# Customer group 1 (15%): pays within 7 days
# Customer group 2 (75%): pays at the end of payment target
# Customer group 3 (10%): pays 14 days after payment target

customers <- c(1001:1100)
rn        <- runif(100)
cg        <- data.frame(Customer=customers, Customer.Group=rn)
cg1   <- cg$Customer[cg$Customer.Group <= 0.15]
cg2   <- cg$Customer[cg$Customer.Group > 0.15 & cg$Customer.Group <=0.90]
cg3   <- cg$Customer[cg$Customer.Group > 0.90]


nr1   <- 600
cg1so <- cg1[round(runif(nr1,1,length(cg1)))]
d1    <- d0 + round(runif(nr1, min = date.min-d0, max = date.max-d0))
pd1   <- 7 - rpois(nr1, lambda=1)
p1    <- round(runif(nr1)*10000+18000)

nr2   <- 1000
cg2so <- cg2[round(runif(nr2,1,length(cg2)))]
d2    <- d0 + round(runif(nr2, min = date.min-d0, max = date.max-d0))
# pd2   <- 18 + round(rnorm(nr2, mean=0, sd=4))
# pd2   <- 18 + round(rnorm(nr2, mean=0, sd=10))
pd2   <- rpois(nr2, lambda=18)
p2    <- round(runif(nr2)*10000+18000)

nr3   <- 400
cg3so <- cg3[round(runif(nr3,1,length(cg3)))]
d3    <- d0 + round(runif(nr3, min = date.min-d0, max = date.max-d0))
# pd3   <- 35 - rpois(nr3, lambda=2)
pd3   <- 35 - rpois(nr3, lambda=10)
p3    <- round(runif(nr3)*10000+18000)

SO1 <- data.frame(BuyerRef=cg1so, DeliveryDate=d1, TotalPrice=p1, PaymentDuration=pd1)
SO2 <- data.frame(BuyerRef=cg2so, DeliveryDate=d2, TotalPrice=p2, PaymentDuration=pd2)
SO3 <- data.frame(BuyerRef=cg3so, DeliveryDate=d3, TotalPrice=p3, PaymentDuration=pd3)

SalesOrder <- rbind(SO1, SO2, SO3)
SalesOrder <- SalesOrder[order(SalesOrder$DeliveryDate),]


# SalesOrder$DeliveryMonth      <- as.numeric(format(SalesOrder$DeliveryDate, format="%m"))
# SalesOrder$DeliveryWeekday    <- as.numeric(format(SalesOrder$DeliveryDate, format="%w"))
# SalesOrder$DeliveryWeekOfYear <- as.numeric(format(SalesOrder$DeliveryDate, format="%W"))

# Prepare for customer specific analysis
# Time Series Objects
Pd.ts     <- aggregate(x=SalesOrder$PaymentDuration, by=list(Customer=SalesOrder$BuyerRef), FUN=ts)
names(Pd.ts) <- c("Customer", "TimeSeries")

# Mean value of time series
# Standard deviation of time series
# Number of Deliveries
# for R "runaways" :-)
Pd <- cbind(Pd.ts, 
            Mean  =sapply(Pd.ts[, "TimeSeries"], FUN=mean), 
            StdDev=sapply(Pd.ts[, "TimeSeries"], FUN=sd),
            NrDel =sapply(Pd.ts[, "TimeSeries"], FUN=length)
            )

# sort the columns (Attribute "TimeSeries" at the end -> printout looks much nicer)
Pd <- Pd[, c(1,3,4,5,2)]


# show customer clusters
cl.m <- Mclust(Pd[,2:4])
plot(cl.m, Pd[,2:4], what=c("classification"))

# add clusters
Pd.cl <- cbind( Pd, cg=cl.m$classification)

cg.cl <- Pd.cl[, c("Customer", "cg")]


# save dataset
write.csv2(SalesOrder, file="data/SalesOrders.csv", quote=FALSE, row.names=FALSE)

