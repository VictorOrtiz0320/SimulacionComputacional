MC.pi=function(){
  #runif samples from a uniform distribution
  xs <- runif(runs,min=-0.5,max=0.5)
  ys <- runif(runs,min=-0.5,max=0.5)
  in.circle <- xs^2 + ys^2 <= 0.5^2
  mc.pi <- (sum(in.circle)/runs)*4
  return(mc.pi)
}
resultados=data.frame()
replicas=30
valorpi=3.14159265358979323846
suppressMessages(library(doParallel))
registerDoParallel(makeCluster(detectCores() - 1))

for (runs in seq(1000,5000,1000)){
  
  
  for (r in 1:replicas) {
    
    InicioTime=Sys.time()
    montecarlo <- foreach(i = 1:runs, .combine=c) %dopar% MC.pi()
    resul <- sum(montecarlo) /runs
    E=abs(pi-resul)
    Ftime=Sys.time()
    TiempoTotal=(Ftime-InicioTime)
    resultados=rbind(resultados,c(runs,E,TiempoTotal))
    
    
  }
  
}
stopImplicitCluster() 

names(resultados)=c("N.corridas","Error","Tiempo")
resultados$N.corridas=as.factor(resultados$N.corridas)
png("screenpi.png",width = 1200,height = 1800, pointsize = 20)
split.screen(c(2,1))
screen(1)
boxplot(data=resultados,Tiempo~N.corridas, xlab="Número de corridas",ylab="Tiempo(s)",main="Tiempo para calcular aproximación",ylim=c(0,1.5),col=(c("Green","orange","red","blue","yellow")))
screen(2)
boxplot(data=resultados,Error~N.corridas, xlab="Número de corridas",ylab="Error",main="Error en la aproximación",col=(c("Green","orange","red","blue","yellow")))
dev.off()