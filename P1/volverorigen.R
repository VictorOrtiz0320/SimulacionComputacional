pos=rep(0,6)
n=length(pos)
n
origen=c(pos)
m=length(origen)
m
contador=0 
for (t in 1:10) {
        cambiar <- sample(1:6, 1)
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

contador
origen