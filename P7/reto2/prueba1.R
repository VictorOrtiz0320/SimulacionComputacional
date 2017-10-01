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
low <- -3
high <- 3
tmax <- 100
x <- runif(1, low, high)
step <- 0.3
#Temp=1000
e=0.995
d=0

for (e in seq(0.995,0.999,0.002)){
 
    resuldatos=data.frame()
  
  for (Temp in seq(1,100,10)){ 
    re=cbind(Temp)
    
    for (tiempo in 1:tmax) {
      resul=data.frame()
     
  
      valor=runif(1)
      prob=exp(-d/Temp)
      delta <- runif(1, -step, step)
      xp <- x + delta
      d=f(xp)-f(x)
      resul=cbind(tiempo,x,xp,Temp)
      
    
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

names(resuldatos)=c("pasos","xi","x prima","Ti","delta","T","N. Aleatorio","exp(-d/T)","xf","f(x)")
names(maxpaso)=c("E","Ti","T","Vmax")
maxpaso$E=as.factor(maxpaso$E)


#png("p7t.png",width = 500,height = 500)
xyplot(data=maxpaso,Vmax~Ti |E ,groups = E,
       panel=function(x,y,subscripts,groups){
         panel.grid(h=-1,v=-1)
         panel.xyplot(x,y)
         panel.stripplot(x,y,
                         groups = groups, subscripts = subscripts,pch=19,type="o")
         #panel.abline(h=wolfram,col="Green",lwd=2)
       })
#graphics.off()

#boxplot(data=maxpaso,Vmax~E)
