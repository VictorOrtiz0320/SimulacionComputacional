repetir <- 1
duracion <- 500
library(parallel)
 
cluster <- makeCluster(detectCores() - 1)
clusterExport(cluster, "duracion")
datos <-  data.frame()
 
for (dimension in 1:2) {
    clusterExport(cluster, "dimension")
    resultado <- parSapply(cluster, 1:repetir,
                           function(r) {
						pos=rep(0,dimension)
						n=length(pos)
						n
						origen=c(1,2)
						m=length(origen)
						m
						contador=0 
						for (t in 1:duracion) {
      					cambiar <- sample(1:dimension, 1)
        					cambio <- 1
        					if (runif(1) < 0.5) {
            				cambio <- -1
        					}
						pos[cambiar]= pos[cambiar]+ cambio
						if (all (pos==origen)){
						contador=contador +1 
						}
						else{
						contador=contador
						}}
						return(contador)
						})
    datos <- rbind(datos, resultado)
}
stopCluster(cluster)
    png("p1er.png")
    boxplot(data.matrix(datos), use.cols=FALSE, 
       xlab="Dimensi\u{F3}n", ylab="N. veces vuelve al origen", 
       main="Practica 1")
 
graphics.off()