x <- rnorm(1000, mean=10, sd=5)
hist(x,
     breaks = 50,
     freq   = FALSE,
     main   = "Gauss-Verteilung")
curve(dnorm(x, mean=10, sd=5), col="red", add=TRUE)