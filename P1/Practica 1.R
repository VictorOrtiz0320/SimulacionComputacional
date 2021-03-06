IniciaProceso = proc.time()
repetir <- 200 #N. de veces a repetir por cada dimensi�n#
duracion <- 100 #Cantidad de pasos o cambios en el vector pos por cada dimensi�n#
#Biblioteca para poder facilitar la ejecuci�n paralela en R#
library(parallel)

#Ayuda a verificar el n�mero de nucleos del procesador#
cluster <- makeCluster(detectCores() - 1)
#Modificamos el numero de cluster a ocupar ya que no mediremos distancias, solo duraci�n# 
clusterExport(cluster, "duracion")
datos <-  data.frame()

#Especificamos el n�mero de dimensiones de trabajo# 
for (dimension in 1:8) {
    clusterExport(cluster, "dimension")
    resultado <- parSapply(cluster, 1:repetir,
                           function(r) {
	#Se genera el vector posici�n#
						pos=rep(0,dimension)
      #Generamos un vector origen para poder hacer la comparaci�n de las veces que vuelve al origen#
						origen=rep(0,dimension)
	#Iniacializamos una variable para llevar la cuenta de cuantas veces vuelve al origen						
						contador=0 
						for (t in 1:duracion) {
      					cambiar <- sample(1:dimension, 1)
        					cambio <- 1
        					if (runif(1) < 0.5) {
            				cambio <- -1
        					}
						pos[cambiar]= pos[cambiar]+ cambio
	#Modificamos esta parte del codigo para lograr realizar una comparaci�n entre la posici�n actual y nuestro vector origen#		
					if (all (pos==origen)){
						contador=contador +1 
						}
						}
						return(contador)
						})
    datos <- rbind(datos, resultado)
}
stopCluster(cluster)
    png("p1er.png")
    boxplot(data.matrix(datos), use.cols=FALSE, xlab="Dimensi\u{F3}n", ylab="N. veces vuelve al origen", main="Practica 1")
 
graphics.off()
Tejecucion=proc.time()-IniciaProceso
print(Tejecucion)  
