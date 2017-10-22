suppressMessages(library(doParallel))
registerDoParallel(makeCluster(detectCores() - 1))
library(ggplot2) # recordar instalar si hace falta
Resultados=data.frame()
Toriginal=numeric()
Final<- data.frame()
Resul<- data.frame()
for (n in c(200)){
  for (k in seq(2,12,2)){
    
      
      source('~/GitHub/SimulacionComputacional/P11/P11.R', encoding = 'UTF-8')
      Toriginal=cbind(k,n)
      Resultados=rbind(Resultados,Toriginal)
     
       data <- data.frame(pos=rep(0, n), dom=dominadores)
       Resul<-cbind(Resul,Resultados, data)
      gr <- ggplot(data, aes(x=pos, y=dom)) + geom_violin(fill="orange", color="red")
      gr + geom_boxplot(width=0.2, fill="blue", color="white", lwd=2) +
        xlab("") +
        ylab("Frecuencia") +
        ggtitle("Cantidad de soluciones dominantes")
      ggsave(file=paste("p11_violin", k,".png", sep='')) #Nombre del jpeg
            
  }
}
stopImplicitCluster() 
#Final<-rbind(Final,data,Resultados)
#names(Resultados)=c("nObjetivos","nSoluciones")
