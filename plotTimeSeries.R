parSave <- par(no.readonly=TRUE)
on.exit(par(parSave))

Pd.fts <- lapply(Pd[,5], FUN=moving.average)

par(mfrow=c(4,3))
for(cust in 1:length(Pd[,1])) {
# for(cust in 1:12) {
  # TimeSeries
  cust.ts <- Pd[,5][[cust]]
  plot(cust.ts, main=paste("Customer", Pd[cust,1]), 
       xlab="Index of ordered DeliverDate",
       ylab="PaymentDuration",
       ylim=c(0,40)
  )
  # Moving average 
  lines(Pd.fts[cust][[1]], col="red")
}



