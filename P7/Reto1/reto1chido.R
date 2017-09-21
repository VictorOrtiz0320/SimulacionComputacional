library(lattice)
library(latticeExtra)
library(reshape2) 

f <- function(x, y) {
  return(((x + 0.5)^4 - 30 * x^2 - 20 * x + (y + 0.5)^4 - 30 * y^2 - 20 * y)/100)
}
x <- seq(-6, 5, 0.25)
y <-  x
z <- outer(x, y, f)
colnames(z)=x
rownames(z)=y
d <- melt(z)
names(d) <- c("x", "y", "z")
png("p7_flat_2.png", width=500, height=500)
levelplot(z ~ x * y, data = d)
graphics.off()
#################################################################################
low <- -2
high <- 3
step <- 0.25
replicas <- 10
best=c()
t=50

currx <- runif(1, low, high)
curry <- runif(1, low, high)
best <- c(currx,curry)


for (pasos in 1:t) {
  
  #p2 <- xyplot(curry~ currx, pch = 4, col = "orange")
  #super=paisaje + as.layer(p2)
  trellis.device(device="png", filename=paste("xyplot",pasos,".png"))
   print( levelplot(z ~ x * y, data = d) + as.layer( xyplot(curry~ currx, pch = 16, col = "red")))
   dev.off()
 
  graphics.off()
  deltax <- runif(1, 0, step)
  deltay <- runif(1, 0, step)
  left <- currx - deltax
  right <- currx + deltax
  top <- curry + deltay
  bot <- curry - deltay
  
  if (f(left,curry) > f(right,curry)) {
    bestx <- c(left,curry)
  } else {
    bestx <-c(right,curry)
  }
  if (f(currx,top) > f(currx,bot)) {
    besty <- c(currx,top)
  } else {
    besty <- c(currx,bot)
  }

  if (f(bestx[1],bestx[2])> f(besty[1],besty[2]))
  {  currx=bestx[1]
  curry=bestx[2]
  } else {
    currx=besty[1]
    curry=besty[2]
  }
  
  if (f(currx,curry) > f(best[1],best[2])) {
    best <- c(currx,curry)
  }
  
  
}

