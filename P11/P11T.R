suppressMessages(library(doParallel))
registerDoParallel(makeCluster(detectCores() - 1))
Resultados=data.frame()
Toriginal=numeric()
Tparalelo=numeric()

for (k in c(10)){
  for (n in seq(200,1000,200)){
    for(r in 1:10){
    
    source('~/GitHub/SimulacionComputacional/P11/Original.R', encoding = 'UTF-8')
    Toriginal=cbind(k,n,"o",Tiempo)
    
    
    source('~/GitHub/SimulacionComputacional/P11/P11.R', encoding = 'UTF-8')
    Tparalelo=cbind(k,n,"p",Tiempo)
    Resultados=rbind(Resultados,Toriginal,Tparalelo)
    }
  }
}
stopImplicitCluster() 
names(Resultados)=c("nObjetivos","nSoluciones","tipo","Tiempo")
Resultados$Tiempo<-as.numeric(levels(Resultados$Tiempo))[Resultados$Tiempo]
Resultados$tipo=as.factor(Resultados$tipo)
#Resultados[Resultados$Tiempo>10,3]<-Resultados[Resultados$Tiempo>10,3]/60
png("P11T.png",width=800, height=1000,pointsize = 15)
boxplot(Tiempo~tipo*nSoluciones,data=Resultados,col = "lightgray",border=c("Green","Blue"),xlab="Número de soluciones ",ylab="Tiempo (min)")
legend("topright", inset=.02,
       c("Original","Paralelizado"), fill=c("Green","Blue"), horiz=TRUE, cex=0.8,box.lty = 0)

graphics.off()
save.image(file = "Practica11chido.RData")