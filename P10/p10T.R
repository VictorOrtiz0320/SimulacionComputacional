Resultados=data.frame()
Toriginal=numeric()
Tparalelo=numeric()

for (init in c(2,5,10)){
  for (r in 1:2){
  
    source('~/GitHub/SimulacionComputacional/P10/p10sp.R')
    Toriginal=cbind(init,"o",Tiempo)
    
    png(paste("p10original",init,r,".png", sep = ""), width=600, height=300)
    plot(1:tmax, mejores, xlab="Paso", ylab="Mayor valor", type='l', ylim=c(0.95*min(mejores), 1.05*optimo))
    points(1:tmax, mejores, pch=15)
    abline(h=optimo, col="green", lwd=3)
    graphics.off()
    
    
    source('~/GitHub/SimulacionComputacional/P10/p10.R')
    Tparalelo=cbind(init,"p",Tiempo)
    Resultados=rbind(Resultados,Toriginal,Tparalelo)
    
    png(paste("p10",init,r,".png", sep = ""), width=600, height=300)
    plot(1:tmax, mejores, xlab="Paso", ylab="Mayor valor", type='l', ylim=c(0.95*min(mejores), 1.05*optimo))
    points(1:tmax, mejores, pch=15)
    abline(h=optimo, col="green", lwd=3)
    graphics.off()
    
  }
  
}
names(Resultados)=c("poblacion","tipo","Tiempo")
Resultados$Tiempo<-as.numeric(levels(Resultados$Tiempo))[Resultados$Tiempo]
Resultados$tipo=as.factor(Resultados$tipo)
Resultados[Resultados$Tiempo>10,3]<-Resultados[Resultados$Tiempo>10,3]/60
png("P10T.png",width=600, height=800,pointsize = 15)
boxplot(Tiempo~tipo*poblacion,data=Resultados,col = "lightgray",border=c("Green","Blue"),xlab="Población ",ylab="Tiempo (min)")
legend("topleft", inset=.02,
       c("Original","Paralelizado"), fill=c("Green","Blue"), horiz=TRUE, cex=0.8,box.lty = 0)

graphics.off()
save.image(file = "Practica10chido.RData")