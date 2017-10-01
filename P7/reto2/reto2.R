f <- function(x) {
  return(cos(14.5*x - 0.3) + x * (x + 0.2) + 1.01)
}
low <- -3
high <- -low
xg <- seq(low, high, 0.05)
y <- f(xg)
tmax <- 100
digitos <- floor(log(tmax, 10)) + 1
step <- 0.3
Temp=5
e=0.995
d=0

x <- runif(1, low, high)
best=x
#############################################################################
for (tiempo in 1:tmax) {
 
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
  if (f(x)>f(best)){
  best=x
  }
#############################################################################
#for (tiempo in 1:tmax) {
  #delta <- runif(1, 0, step)
  #left <- curr - delta
  #right <- curr + delta
  #fl <- f(left)
  #fr <- f(right)
  #if (fl < fr) {
   # curr <- left
  #} else {
   # curr <- right
  #}
  #if (f(curr) < f(best)) { # minimizamos
    #best <- curr
  #}
  #############################################################################
  tl <- paste(tiempo, "", sep="")
  while (nchar(tl) < digitos) {
    tl <- paste("0", tl, sep="")
  }
  salida <- paste("p7_t", tl, ".png", sep="")
  tiempo <- paste("Paso", tiempo)
  png(salida, width=500, height=400)
  plot(xg, y, type="l")
  abline(v = best, col="green", lwd=2)
  points(x, f(x), pch=16, col="red")
  graphics.off()
}