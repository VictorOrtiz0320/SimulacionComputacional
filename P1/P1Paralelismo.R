IniciaProceso = proc.time()
repetir <- 10 #N. de veces a repetir por cada dimensión#
duracion <- 500 #Cantidad de pasos o cambios en el vector pos por cada dimensión#
#Biblioteca para poder facilitar la ejecución paralela en R#
library(parallel)

#Ayuda a verificar el número de nucleos del procesador#
cluster <- makeCluster(detectCores() - 1)
#Modificamos el numero de cluster a ocupar ya que no mediremos distancias, solo duración# 
clusterExport(cluster, "duracion")
datos <-  data.frame()

#Especificamos el número de dimensiones de trabajo# 
for (dimension in 1:8) {
    clusterExport(cluster, "dimension")
    resultado <- parSapply(cluster, 1:repetir,
                           function(r) {
	#Se genera el vector posición#
						pos=rep(0,dimension)
      #Generamos un vector origen para poder hacer la comparación de las veces que vuelve al origen#
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
	#Modificamos esta parte del codigo para lograr realizar una comparación entre la posición actual y nuestro vector origen#		
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
#User time es el tiempo de la CPU dedicado a la ejecución del las instrucciones del proceso. System time es el tiempo de la CPU empleado por el sistema operativo (el núcleo o kernel) siguiendo las instrucciones del proceso (abrir ficheros, iniciar otros procesos o mirar al reloj del sistema, etc.). Elapsed time es el tiempo transcurrido 'real' desde que se inició el proceso.#
FinalProceso=proc.time()-IniciaProceso
print(FinalProceso)  
datos