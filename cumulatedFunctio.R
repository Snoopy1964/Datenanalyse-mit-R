# sum plot, for smeared amounts (amount*norm)
# f.single <- function(td) {td[2]*dnorm(t[1], mean=[td[3]], sd=td[4])}
p.single <- function(x, a=1, m=0, sd=1) {  a*pnorm(x, mean=m, sd=sd)}

p.cum    <- function(x, aa, mm, sdsd) {
  df    <- data.frame(a=aa, m=mm, sd=sdsd)
  y.cum <- vector(mode='numeric', length=0)
  for(xt in x) {
    df$t  <- rep(xt, length(aa)) 
    y.cum <- c(y.cum, sum(transform(df, pA=p.single(t,a,m,sd))[, "pA"]))
  }
  return(y.cum)
}
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

aa   <- c(1000,2000,3000)
mm   <- c(10,20,30)
sdsd <- c(10,10,10)

tt   <- 10*1:10

testIt.p <- p.cum(tt, aa, mm, sdsd)
testIt.s <- f.sum(tt, aa, mm, sdsd)

tt <- seq(as.numeric(as.Date("2012-07-01")),as.numeric(as.Date("2012-10-01")),0.1)
y.p <- p.cum(tt,SO.test[,"TotalPrice"], as.numeric(SO.test[,"predict.date"]),SO.test[,"predict.sd"])
y.s <- f.sum(tt,SO.test[,"TotalPrice"], as.numeric(SO.test[,"predict.date"]),SO.test[,"predict.sd"])
tt <- as.Date(tt, origin="1970-01-01")
