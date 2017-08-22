IniciaProceso = proc.time()
library(parallel)
dim <- 10
num <-  dim^2
<<<<<<< HEAD
repetir=200
yi=matrix(rep(0),nrow=repetir,ncol = 9)

=======
repetir=3
x=0
yi=matrix(rep(0),nrow=repetir,ncol = 9)
datos=data.frame()
for (probabilidad in 1:9){
  
  proba=probabilidad/10
  res=1-proba
  for (i in 1:repetir){
  x[probabilidad]=c(proba)  
actual=matrix(sample(c(1:0),num, prob=c(proba,res),replace=TRUE),nrow=dim, ncol=dim)
suppressMessages(library("sna"))
png("p2_t0.png")
plot.sociomatrix(actual, diaglab=FALSE, main="Inicio")
graphics.off()
 
>>>>>>> 365dedebbc1763e08bd0a78833b287be4526abdd
paso <- function(p) {
    fila <- floor((p - 1) / dim) + 1
    columna <- ((p - 1) %% dim) + 1
    vecindad <-  actual[max(fila - 1, 1) : min(fila + 1, dim),max(columna - 1, 1): min(columna + 1, dim)]
    return(1 * ((sum(vecindad) - actual[fila, columna]) == 3))
}

cluster <- makeCluster(detectCores() - 1)
clusterExport(cluster, "dim")
clusterExport(cluster, "paso") 


<<<<<<< HEAD
for (probabilidad in 1:9){
  proba=probabilidad/10
  
  for (i in 1:repetir){
  	
	actual=matrix(sample(c(1:0),num, prob=c(proba,(1-proba)),replace=TRUE),nrow=dim, ncol=dim)
  	#suppressMessages(library("sna"))
  	#png("p2_t0.png")
  	#plot.sociomatrix(actual, diaglab=FALSE, main="Inicio")
  	#graphics.off()	

   	for (iteracion in 1:9) {
     
    		clusterExport(cluster, "actual")
    		siguiente <- parSapply(cluster, 1:num, paso)
    		if (sum(siguiente) == 0) { # todos murieron
        	#print("Ya no queda nadie vivo.")
  
        	break;
    		}
  		actual <- matrix(siguiente, nrow=dim, ncol=dim, byrow=TRUE)
  		#salida = paste("p2_t", iteracion, ".png", sep="")
  		#tiempo = paste("Paso", iteracion)
  		#png(salida)
  		#plot.sociomatrix(actual, diaglab=FALSE, main=tiempo)
  		#graphics.off()
  	}


    #plot.sociomatrix(actual, diaglab=FALSE, main=tiempo)
    yi[i,probabilidad]=iteracion
 
  } 
}
stopCluster(cluster)
colnames(yi)=c(0.10,0.20,0.30,0.40,0.50,0.60,0.70,0.80,0.9)
boxplot(yi,xlab="Probabilidades",ylab="Número de pasos",main="Práctica 2")
=======
	
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



print(iteracion)

stopCluster(cluster)
plot.sociomatrix(actual, diaglab=FALSE, main=tiempo)


yi[i,probabilidad]=iteracion 
 

  }
  
 
}

#datos=data.frame(yi)
boxplot(yi,xlab="Probabilidades",ylab="Número de pasos",main="Práctica 2")

>>>>>>> 365dedebbc1763e08bd0a78833b287be4526abdd


Tejecucion=proc.time()-IniciaProceso
print(Tejecucion)  