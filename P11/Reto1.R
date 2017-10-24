suppressMessages(library(doParallel))
registerDoParallel(makeCluster(detectCores() - 1))
Tinicial=Sys.time()
pick.one <- function(x) {
  if (length(x) == 1) {
    return(x)
  } else {
    return(sample(x, 1))
  }
}

poli <- function(maxdeg, varcount, termcount) {
  f <- data.frame(variable=integer(), coef=integer(), degree=integer())
  for (t in 1:termcount) {
    var <- pick.one(1:varcount)
    deg <- pick.one(1:maxdeg)
    f <-  rbind(f, c(var, runif(1), deg))
  }
  names(f) <- c("variable", "coef", "degree")
  return(f)
}

eval <- function(pol, vars, terms) {
  value <- 0.0
  for (t in 1:terms) {
    term <- pol[t,]
    value <-  value + term$coef * vars[term$variable]^term$degree
  }
  return(value)
}

domin.by <- function(target, challenger, total) {
  if (sum(challenger < target) > 0) {
    return(FALSE) # hay empeora
  } # si no hay empeora, vemos si hay mejora
  return(sum(challenger > target) > 0)
}
vc <- 4
md <- 3
tc <- 5
k <- 2 # cuantas funciones objetivo

Fobjetivo<- function(i){ 
  
  return(poli(md,vc, tc))
}
obj<-foreach(i=1:k) %dopar% Fobjetivo(i)
minim <- (runif(k) > 0.5)
sign <- (1 + -2 * minim)
n <- 200 # cuantas soluciones aleatorias


Fsoluciones<- function(i){ # evaluamos las soluciones
  resul<-double()
  for (j in 1:k) { # para todos los objetivos
    datos<-eval(obj[[j]], sol[i,], tc)
    resul<-cbind(resul,datos)
  }
  return(resul)
}
sol <- matrix(runif(vc * n), nrow=n, ncol=vc)
val <- matrix(rep(NA, k * n), nrow=n, ncol=k)
val<-foreach(i=1:n, .combine = rbind) %dopar% Fsoluciones(i)

Fpareto<- function(i){
  no.dom <- logical()
  dominadores <- integer()
  d <- logical()
  for (j in 1:n) {
    d <- c(d, domin.by(sign * val[i,], sign * val[j,], k))
  }
  cuantos <- sum(d)
  return(cuantos==0)
}
no.dom<-foreach(i=1:n, .combine = c) %dopar% Fpareto(i)
stopImplicitCluster() 
frente<- subset(val, no.dom) # solamente las no dominadas
tam<-dim(frente)[1]

cual <- c("max", "min")
xl <- paste("1° objetivo (", cual[minim[1] + 1], ")", sep="")
yl <- paste("2° objetivo (", cual[minim[2] + 1], ")", sep="")

#RETO 1  

png("Reto1.png",width = 600,height = 900,pointsize=12)
split.screen(c(3,1))
split.screen(c(1,2), screen = 1)
#split.screen(c(1,2), screen = 2)
#split.screen(c(1,2), screen = 3)
screen(1)
plot(val[,1], val[,2], xlab=xl,
     ylab=yl)
points(frente[,1], frente[,2], col="red", pch=19, cex=1)

if (tam>2){ #Determinar si el frente tiene más de dos puntos 
  frente<-as.data.frame(frente)
  colnames(frente)<-c("x","y")
  nFrente<-frente[order(frente$x),] #Ordenar de mejor a peor en función de una función objetivo x
   
# Calcular las distancias entre cada uno de los puntos del frente
  separacion<-c()
  for (i in 1:tam-1){
    s<- sqrt((nFrente[i,]$x-nFrente[i+1,]$x)^2+(nFrente[i,]$y-nFrente[i+1,]$y)^2)
    separacion<-c(separacion,s)
  }
  umbral<-mean(separacion) #distancia umbral
  
  conservar<-rep(FALSE,tam)
  for (i in 1:tam){
    if (nFrente[i,]==head(nFrente,n=1)||nFrente[i,]==tail(nFrente,n=1)){
      conservar[i]=TRUE}
    else{
        j<-max(which(conservar))
        s<-sqrt((nFrente[i,]$x-nFrente[j,]$x)**2+(nFrente[i,]$y-nFrente[j,]$y)**2)
        if(s>=umbral){
          conservar[i]=TRUE
         }
        else{
          conservar[i]=FALSE
          }
      }
  }

Fdiversificado<-subset(nFrente,conservar)
screen(2)
plot(val[,1], val[,2], xlab=xl,
     ylab=yl)
points(frente[,1], frente[,2], col="red", pch=19, cex=1)
points(Fdiversificado[,1], Fdiversificado[,2], col="green", pch=19, cex=1)
screen(3)
plot(val[,1], val[,2], xlab=xl,
     ylab=yl)
points(Fdiversificado[,1], Fdiversificado[,2], col="green", pch=19, cex=1)
}
dev.off()
Tfinal=Sys.time()
Tiempo=Tfinal-Tinicial
print(Tiempo)

