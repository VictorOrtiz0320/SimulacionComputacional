IniciaProceso = proc.time()
repetir <- 10 #N. de veces a repetir por cada dimensión#
duracion <- 50 #Cantidad de pasos o cambios en el vector pos por cada dimensión#

datos <-  data.frame()

#Especificamos el número de dimensiones de trabajo# 
for (dimension in 1:500) {
	resultado=sapply (1:repetir,function(r) {
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

    png("p1er.png")
    boxplot(data.matrix(datos), use.cols=FALSE, xlab="Dimensi\u{F3}n", ylab="N. veces vuelve al origen", main="Practica 1")
 
graphics.off()
FinalProceso=proc.time()-IniciaProceso
print(FinalProceso)  
datos