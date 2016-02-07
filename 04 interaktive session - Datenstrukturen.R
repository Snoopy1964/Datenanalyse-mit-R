# Data structures
a <- c(1,2,4,4)
b <- c(3,5,2,4)
a*b
seq(-1,1,0.2)
1:5           # same as seq(1,5,1)

# data.frame
n <- c("Merkur","Venus","Erde","Mars")
r <- c(2439, 6052, 6378, 3397)
m <- c(0,0,1,2) 
planeten <- data.frame(name=n, aequator.radius=r, anzahl.monde=m)
planeten

# Factors
n <- factor(c("Merkur","Venus","Erde","Mars"))
r <- c(2439, 6052, 6378, 3397)
m <- c(0,0,1,2) 
s <- ordered(c("klein", "groß", "groß", "mittel"),
            levels=c("klein", "mittel", "groß"))
planeten <- data.frame(name=n, aequator.radius=r, anzahl.monde=m, 
                       groessen.kategorie=s)
planeten