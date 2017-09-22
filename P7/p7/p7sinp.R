f <- function(x, y) {
  return(((x + 0.5)^4 - 30 * x^2 - 20 * x + (y + 0.5)^4 - 30 * y^2 - 20 * y)/100)
}


low <- -2
high <- 4
step <- 0.25
replicas <- 5
best=c()
t=100
resul=data.frame()
resuldatos=data.frame()

for (caminatas in 1:replicas){
  currx <- runif(1, low, high)
  curry <- runif(1, low, high)
  best <- c(currx,curry)

  for (tiempo in 1:t) {
  
    deltax <- runif(1, 0, step)
    deltay <- runif(1, 0, step)
    left   <- currx - deltax
    right  <- currx + deltax
    top    <- curry + deltay
    bot    <- curry - deltay
  
    if (f(left,curry) > f(right,curry)) {
      bestx <- c(left,curry)
    } 
    else {
      bestx <-c(right,curry)
    }
    if (f(currx,top) > f(currx,bot)) {
      besty <- c(currx,top)
    } 
    else {
      besty <- c(currx,bot)
    }
  
    if (f(bestx[1],bestx[2])> f(besty[1],besty[2])) {
     currx=bestx[1]
     curry=bestx[2]
    }
    else { 
      currx=besty[1]
      curry=besty[2]
    }
  
    if (f(currx,curry) > f(best[1],best[2])) {
      best <- c(currx,curry)
    }
    resul=cbind(best[1],best[2],f(best[1],best[2]))
    resuldatos=rbind(resuldatos,resul)
    
  }
  
}