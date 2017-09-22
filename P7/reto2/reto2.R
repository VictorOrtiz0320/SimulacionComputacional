library(lattice)
library(reshape2) 
f <- function(x) {
  return(cos(14.5*x - 0.3) + x * (x + 0.2) + 1.01)
}
maxpaso=data.frame()
maxchido=data.frame()
#resul=data.frame()
#resuldatos=data.frame()
#maxpaso=data.frame()
#maxchido=data.frame()
low <- -7.7
high <- 7.8
tmax <- 100
x <- runif(1, low, high)
step <- 0.3
#Temp=1000
e=0.995
d=0

for (e in seq(0.991,0.999,0.002)){
 
 
  
  for (Temp in seq(100,1000,50)){ 
    re=cbind(Temp)
    for (tiempo in 1:tmax) {
      resul=data.frame()
      resuldatos=data.frame()
  
      valor=runif(1)
      prob=exp(-d/Temp)
      delta <- runif(1, -step, step)
      xp <- x + delta
      d=f(xp)-f(x)
      resul=cbind(x,xp,Temp)
      
    
      if (d>0){
        x=xp
      }
      else{
        if  (valor<prob){
        Temp=Temp*e
        x=xp
        }
      }
    
    resul=cbind(resul,d,Temp,valor,prob,x,f(x))
    resuldatos=rbind(resuldatos,resul)
   
  
  
    
    }
    maxchido=cbind(e,re,Temp,max(resuldatos[9]))
    maxpaso=rbind(maxpaso,maxchido)
  
  }
 
}

names(resuldatos)=c("xi","x prima","Ti","delta","T","N. Aleatorio","exp(-d/T)","xf","f(x)")
names(maxpaso)=c("E","Ti","T","Vmax")
maxpaso$E=as.factor(maxpaso$E)

R1=maxpaso[maxpaso$E==0.997,]

xyplot(data=maxpaso,Vmax~Ti ,type="o")
xyplot(data=R1,Vmax~Ti ,type="o")

