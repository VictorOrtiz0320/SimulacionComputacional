f <- function(x) {
  return(cos(14.5*x - 0.3) + x * (x + 0.2) + 1.01)
}

resul=data.frame()
resuldatos=data.frame()
low <- -7.7
high <- 7.8
tmax <- 100
x <- runif(1, low, high)
step <- 0.3
t=100
e=0.995
d=0

for (tiempo in 1:tmax) {
  valor=runif(1)
  prob=exp(-d/t)
  delta <- runif(1, -step, step)
  xp = x + delta
  d=f(xp)-f(x)
  resul=cbind(x,xp)
  
  if (d>0){
    
    x=xp
  }
  else{
    
    if  (valor<prob){
     t=t*e
     x=xp
  }
  }
  
  
  resul=cbind(resul,d,t,valor,prob,x,f(x))
  resuldatos=rbind(resuldatos,resul)
  
}


names(resuldatos)=c("xi","x prima","delta","T","N. Aleatorio","exp(-d/t)","xf","f(x)")



