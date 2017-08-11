dur=10

contador1=0
contador2=0
contador3=0 

for (dim in 1:3) {
	pos=rep(0,dim)
	origen=c(pos)
	contador=0

	for (t in 1:dur) {
        cambiar <- sample(1:dim, 1)
        cambio <- 1
        if (runif(1) < 0.5) {
            cambio <- -1
        }
	pos[cambiar]= pos[cambiar]+ cambio
   if (all (pos==origen)){
	contador=contador +1 }
   else{
      contador=contador
}
print (pos)
}
print ("N. veces que vuelve al origen")
print (contador)

if (dim==1){
	contador1=contador1+ contador
}
if(dim==2){
	contador2=contador2+ contador
}
if (dim==3){
	contador3=contador3+ contador
}

}
contador1
contador2
contador3
datos=data.frame()
dimensiones=sort(sample(1:dim))
norigen=c(contador1,contador2,contador3)
datos=rbind(norigen,dimensiones)
barplot(norigen,main="Practica 1", xlab="Dimensiones", ylab="N. veces vuelve al origen",names.arg=dimensiones)



