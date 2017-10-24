suppressMessages(library(doParallel))
registerDoParallel(makeCluster(detectCores() - 1))
library(ggplot2)
Resultados=data.frame()
Toriginal=numeric()
n=200

  for (k in seq(2,10,2)){
    for (r in 1:50){
      
      source('~/GitHub/SimulacionComputacional/P11/P11.R', encoding = 'UTF-8')
      Toriginal=cbind(tam,k,r)
      Resultados=rbind(Resultados,Toriginal)
  }
}
stopImplicitCluster() 
names(Resultados)=c("Frente","nObjetivos","Replicas")
Resultados$nObjetivos<- as.factor(Resultados$nObjetivos)
library(ggplot2)
 ggplot(data=Resultados, aes(Resultados$nObjetivos, (Resultados$Frente/n)*100)) +
  geom_violin(scale="width",fill="dodgerblue4", color="black")+
   geom_boxplot(width=0.2, fill="dodgerblue2", color="aliceblue")+ 
 xlab("Número de funciones objetivo k") +
 ylab("Porcentaje de funciones no dominantes (%)")+
   theme_grey()
ggsave(file=paste("p11_violin.png", sep='')) #Nombre del jpeg
