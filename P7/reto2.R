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
t=1
e=0.995

for (tiempo in 1:tmax) {
  delta <- runif(1, -step, step)
  xp <- x + delta
  d=f(xp)-f(x)
  resul=cbind(x,xp)
  
  if (d>0){
    
    x=xp
  }
  else{
    
    if  (runif(1)<exp(-d/t)){
     t=t*e
     x=x
  }
  }
  
  
  resul=cbind(tiempo,resul,d,t,x,f(x))
  resuldatos=rbind(resuldatos,resul)
  
}