#suppressMessages(library(doParallel))
#registerDoParallel(makeCluster(detectCores() - 1))
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
#k <- 2 # cuantas funciones objetivo

Fobjetivo<- function(i){ 
  
  return(poli(md,vc, tc))
}
obj<-foreach(i=1:k) %dopar% Fobjetivo(i)
minim <- (runif(k) > 0.5)
sign <- (1 + -2 * minim)
#n <- 200 # cuantas soluciones aleatorias


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
frente<- subset(val, no.dom) # solamente las no dominadas
tam<-dim(frente)[1]
#stopImplicitCluster() 
Tfinal=Sys.time()
Tiempo=Tfinal-Tinicial
print(Tiempo)
