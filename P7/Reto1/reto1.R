f <- function(x, y) {
  return(((x + 0.5)^4 - 30 * x^2 - 20 * x + (y + 0.5)^4 - 30 * y^2 - 20 * y)/100)
}
#Graficar paisaje
x <- seq(-6, 5, 0.25)
y <-  x
z <- outer(x, y, f)
low <- -2
high <- 3
step <- 0.25
replicas <- 1
best=c()
t=50
k=1
wolfram=0.0666822

BusquedaLocal <- function() {
  library(reshape2) # recuerda instalar paquetes antes de intentar su uso
  library(lattice) # lo mismo aplica con este paquete
  
  
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
#puntos <- plot(data=caminatas,f(x,y)~Paso,type="p")
#points(data=caminatas,f(x,y)~Paso,pch=20,col=c("red","blue","orange","purple","yellow"))
#abline(h=wolfram,col="green",lwd=2)
colnames(z)=x
rownames(z)=y
stopImplicitCluster()



library(latticeExtra)
library(reshape2) # recuerda instalar paquetes antes de intentar su uso
d <- melt(z)
names(d) <- c("x", "y", "z")
library(lattice) # lo mismo aplica con este paquete
paisaje <-levelplot(z ~ x * y, data = d)
#for (graph in 1:t){
paisaje <-levelplot(z ~ x * y, data = d)
  p = xyplot(caminatas[5,3],caminatas[5,4])
  super=paisaje + as.layer(p2)
  png(paste("paisaje",graph,".png"), width = 300,height = 300)
  super
  graphics.off()
#}
  p2 <- xyplot(2.15~ 2, pch = 4, col = "orange")