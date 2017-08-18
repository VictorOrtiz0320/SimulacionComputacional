library(parallel)
dim <- 10
num <-  dim^2
proba=0.20
res=1-proba
repetir=9

actual=matrix(sample(c(1:0),num, prob=c(proba,res),replace=TRUE),nrow=dim, ncol=dim)
#actual <- matrix(round(runif(num)), nrow=dim, ncol=dim)#
suppressMessages(library("sna"))
png("p2_t0.png")
plot.sociomatrix(actual, diaglab=FALSE, main="Inicio")
graphics.off()
 
paso <- function(p) {
    fila <- floor((p - 1) / dim) + 1
    columna <- ((p - 1) %% dim) + 1
    vecindad <-  actual[max(fila - 1, 1) : min(fila + 1, dim),max(columna - 1, 1): min(columna + 1, dim)]
    return(1 * ((sum(vecindad) - actual[fila, columna]) == 3))
}
 
cluster <- makeCluster(detectCores() - 1)
clusterExport(cluster, "dim")
clusterExport(cluster, "paso") 


	
	
for (iteracion in 1:9) {
    clusterExport(cluster, "actual")
    siguiente <- parSapply(cluster, 1:num, paso)
    if (sum(siguiente) == 0) { # todos murieron
        print("Ya no queda nadie vivo.")
        break;
    }
    actual <- matrix(siguiente, nrow=dim, ncol=dim, byrow=TRUE)
    salida = paste("p2_t", iteracion, ".png", sep="")
    tiempo = paste("Paso", iteracion)
    png(salida)
    plot.sociomatrix(actual, diaglab=FALSE, main=tiempo)
    graphics.off()
		
}





stopCluster(cluster)
plot.sociomatrix(actual, diaglab=FALSE, main=tiempo)





