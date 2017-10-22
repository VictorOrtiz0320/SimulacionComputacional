suppressMessages(library(doParallel))
registerDoParallel(makeCluster(detectCores() - 1))
library(ggplot2) # recordar instalar si hace falta
Resultados=data.frame()
Toriginal=numeric()
Final<- data.frame()
Resul<- data.frame()
for (n in c(200)){
  for (k in seq(2,8,1)){
    
      
      source('~/GitHub/SimulacionComputacional/P11/P11.R', encoding = 'UTF-8')
      Toriginal=cbind(dominadores,k,n)
      Resultados=rbind(Resultados,Toriginal)
  }
}
stopImplicitCluster() 

names(Resultados)=c("Dominadores","nObjetivos","nSoluciones")
Resultados$nObjetivos<- as.factor(Resultados$nObjetivos)
library(ggplot2)
 ggplot(data=Resultados, aes(Resultados$nObjetivos, Resultados$Dominadores/n)) +
  geom_violin(scale="width",fill="dodgerblue4", color="black")+
   geom_boxplot(width=0.2, fill="dodgerblue2", color="aliceblue", lwd=2)+ 
 xlab("Número de funciones objetivo k") +
 ylab("Porcentaje de funciones dominantes")
 #ggtitle("Cantidad de soluciones dominantes")
ggsave(file=paste("p11_violin.png", sep='')) #Nombre del jpeg
