
suppressMessages(library(distr))
f <- function(x) { return(1 / (exp(x) + exp(-x))) }
g <- function(x) { return((2 / pi) * f(x)) }
generador  <- r(AbscontDistribution(d = g)) # creamos un generador

desde <- 3
hasta <- 7
pedazo <- 50000
replicas=50
resultados=data.frame()
valor=  0.048834111126

parte <- function() {
  valores <- generador(pedazo)
  return(sum(valores >= desde & valores <= hasta))
}

suppressMessages(library(doParallel))
registerDoParallel(makeCluster(detectCores() - 1))

for (cuantos in seq(100,500,100)){
    
 
 
  
    for (r in 1:replicas) {
      
      InicioTime=Sys.time()
      montecarlo <- foreach(i = 1:cuantos, .combine=c) %dopar% parte()
      integral <- sum(montecarlo) / (cuantos * pedazo)
      resul=c((pi / 2) * integral)
      E=abs(valor-resul)                                                                                                  
      Ftime=Sys.time()
      TiempoTotal=(Ftime-InicioTime)
      resultados=rbind(resultados,c(cuantos,E,TiempoTotal))
     
      
    }
  
}
stopImplicitCluster() 
    
 names(resultados)=c("N.corridas","Error","Tiempo")
 resultados$N.corridas=as.factor(resultados$N.corridas)
 png("screen.png",width = 1200,height = 1800, pointsize = 20)
 split.screen(c(2,1))
 screen(1)
 boxplot(data=resultados,Tiempo~N.corridas, xlab="Número de corridas",ylab="Tiempo(s)",main="Tiempo para calcular aproximación",ylim=c(0,1),col=(c("Green","orange","red","blue","yellow")))
 screen(2)
 boxplot(data=resultados,Error~N.corridas, xlab="Número de corridas",ylab="Error",main="Error en la aproximación",col=(c("Green","orange","red","blue","yellow")))
 dev.off()