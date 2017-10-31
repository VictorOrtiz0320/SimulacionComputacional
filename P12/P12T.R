suppressMessages(library(doParallel))
registerDoParallel(makeCluster(detectCores() - 1))
Resultados=data.frame()
Toriginal=numeric()
Tparalelo=numeric()

  for (a in seq(500,2000,500)){
    for(r in 1:10){
    
    source('~/GitHub/SimulacionComputacional/P12/original.R')
    Toriginal=cbind(a,r,"o",Tiempo,AciertosP)
    
    
    source('~/GitHub/SimulacionComputacional/P12/P12p.R')
    Tparalelo=cbind(a,r,"p",Tiempo,AciertosP)
    Resultados=rbind(Resultados,Toriginal,Tparalelo)
    }
  }
stopImplicitCluster() 
names(Resultados)=c("nPrueba","replica","tipo","Tiempo","Paciertos")
Resultados$Tiempo<-as.numeric(levels(Resultados$Tiempo))[Resultados$Tiempo]
Resultados$Paciertos<-as.numeric(levels(Resultados$Paciertos))[Resultados$Paciertos]
Resultados$tipo=as.factor(Resultados$tipo)
#Resultados[Resultados$Tiempo>10,3]<-Resultados[Resultados$Tiempo>10,3]/60
#Tiempos
png("P12T.png",width=800, height=1000,pointsize = 15)
boxplot(Tiempo~tipo*nPrueba,data=Resultados,col = c("chartreuse","dodgerblue"),border=c("chartreuse4","dodgerblue4"),xlab="Número de soluciones ",ylab="Tiempo (s)")
legend("topleft", inset=.02,
       c("Original","Paralelizado"), fill=c("chartreuse","dodgerblue"), horiz=TRUE, cex=0.8,box.lty = 0)
graphics.off()

#Porcentajes
png("P12P.png",width=800, height=1000,pointsize = 15)
boxplot(Paciertos~tipo*nPrueba,data=Resultados,col = c("chartreuse","dodgerblue"),border=c("chartreuse4","dodgerblue4"),xlab="Número de pruebas",ylab="Porcentaje de acierto (%)")
legend("topright", inset=.02,
       c("Original","Paralelizado"), fill=c("chartreuse","dodgerblue"), horiz=TRUE, cex=0.8,box.lty = 0)

graphics.off()

#Psecuencial<-Resultados[Resultados$tipo=="o",]
#Pparalelo<-Resultados[Resultados$tipo=="p",]


for (a in seq(500,2000,500)){
  PruebaTO<-Resultados[Resultados$nPrueba == a & Resultados$tipo=="p",] 
  PruebaTP<-Resultados[Resultados$nPrueba == a & Resultados$tipo=="o",]
  
  vecO<-PruebaTP$Tiempo
  vecP<-PruebaTO$Tiempo
  student<-t.test(vecO,vecP)
  print(student)

}
save.image(file = "Practica11chido.RData")