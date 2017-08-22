IniciaProceso = proc.time()
library(parallel)
dim <- 10
num <-  dim^2
repetir=1
#Inicializar matriz donde se guardarán los resultados
yi=matrix(rep(0),nrow=repetir,ncol = 9)

paso <- function(p) {
    fila <- floor((p - 1) / dim) + 1
    columna <- ((p - 1) %% dim) + 1
    vecindad <-  actual[max(fila - 1, 1) : min(fila + 1, dim),max(columna - 1, 1): min(columna + 1, dim)]
    return(1 * ((sum(vecindad) - actual[fila, columna]) == 3))
}

cluster <- makeCluster(detectCores() - 1)
clusterExport(cluster, "dim")
clusterExport(cluster, "paso") 

# Ciclo que controla la probabilidad
for (probabilidad in 2:2){
  proba=probabilidad/10
# Ciclo que controla las veces que se repetirá cada corrida por probabilidad  
  for (i in 1:repetir){
  	
	actual=matrix(sample(c(1:0),num, prob=c(proba,(1-proba)),replace=TRUE),nrow=dim, ncol=dim)
  	suppressMessages(library("sna"))
  	png("p2_t0.png")
  	plot.sociomatrix(actual, diaglab=FALSE, main="Inicio")
  	graphics.off()	

   	for (iteracion in 1:9) {
     
    		clusterExport(cluster, "actual")
    		siguiente <- parSapply(cluster, 1:num, paso)
    		if (sum(siguiente) == 0) { # todos murieron
        	#print("Ya no queda nadie vivo.")
  
        	break;
    		}
  		actual <- matrix(siguiente, nrow=dim, ncol=dim, byrow=TRUE)
  	
  	}


  #Llenado de la matriz de resultados
    yi[i,probabilidad]=iteracion
 
  } 
}
stopCluster(cluster)
colnames(yi)=c(0.10,0.20,0.30,0.40,0.50,0.60,0.70,0.80,0.9)
png("p2.png")
boxplot(yi,xlab="Probabilidades",ylab="Número de pasos",main="Práctica 2")
graphics.off()

Tejecucion=proc.time()-IniciaProceso
print(Tejecucion)  