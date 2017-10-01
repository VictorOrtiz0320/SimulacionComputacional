library(lattice)
library(latticeExtra)
library(reshape2) 

f <- function(x, y) {
  return(((x + 0.5)^4 - 30 * x^2 - 20 * x + (y + 0.5)^4 - 30 * y^2 - 20 * y)/100)
}


low <- -2
high <- 3
step <- 0.25
replicas <- 10
best=c()
t=50
k=1
wolfram=0.0666822

BusquedaLocal <- function() {
  
  resul=data.frame()
  resuldatos=data.frame()
  currx <- runif(1, low, high)
  curry <- runif(1, low, high)
  best <- c(currx,curry)
  
 
  
  for (pasos in 1:t) {
  
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
    ### Nuevo ###
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
    resul=cbind(q,pasos,best[1],best[2],f(best[1],best[2]))
    resuldatos=rbind(resuldatos,resul)
  }
 
  return(resuldatos)
  
}

suppressMessages(library(doParallel))
registerDoParallel(makeCluster(detectCores() - 1))
caminatas <- foreach(q=1:replicas, .combine=rbind) %dopar% BusquedaLocal()

names(caminatas)=c("Replicas","Paso","x","y","f(x,y)")
caminatas$Replicas=as.factor(caminatas$Replicas)
stopImplicitCluster()
R1=caminatas[caminatas$Replicas==1,]
R2=caminatas[caminatas$Replicas==2,]
R3=caminatas[caminatas$Replicas==3,]
R4=caminatas[caminatas$Replicas==4,]
R5=caminatas[caminatas$Replicas==5,]

png("p7.png")
#xyplot(data=R1,f(x,y)~Paso,pch = 16, col = "red")
#xyplot(data=R2,f(x,y)~Paso,pch = 12, col = "blue")
plot(data=caminatas,f(x,y)~Paso,type="o")
#points(data=R1,R1$f(x,y), R1$Paso, pch=15, col="red")
points(data=caminatas,f(x,y)~Paso,pch=20,col="blue")
abline(h=wolfram,col="green",lwd=2)
graphics.off()
