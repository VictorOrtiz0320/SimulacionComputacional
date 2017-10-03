
Resultados=data.frame()
Toriginal=numeric()
Tparalelo=numeric()

for (k in c(80000,100000,150000,200000)){
  for (r in 1:5){
  
    source('~/GitHub/SimulacionComputacional/P8/p8original/p8original.R', encoding = 'UTF-8')
    Toriginal=cbind(k,"o",Tiempo)
    
    source('~/GitHub/SimulacionComputacional/P8/p8para/parap8.R')
    Tparalelo=cbind(k,"p",Tiempo)
    Resultados=rbind(Resultados,Toriginal,Tparalelo)
     
  }
  
      


}
names(Resultados)=c("k","tipo","Tiempo")
Resultados$k<-as.numeric(levels(Resultados$k))[Resultados$k]
Resultados$Tiempo<-as.numeric(levels(Resultados$Tiempo))[Resultados$Tiempo]
Resultados$tipo=as.factor(Resultados$tipo)
Resultados[Resultados$Tiempo>10,3]<-Resultados[Resultados$Tiempo>10,3]/60
Resultados$k<-Resultados$k/1000
png("Prac8tiempos.png",width=600, height=800,pointsize = 15)
boxplot(Tiempo~tipo*k,data=Resultados,col=c("Green","Blue"),xlab="Valores de K (10^3)",ylab="Tiempo (min)")
legend("topleft", inset=.02,
       c("Original","Paralelizado"), fill=c("Green","Blue"), horiz=TRUE, cex=0.8,box.lty = 0)

graphics.off()
save.image(file = "Practica8chido.RData")

for (k in c(80,100,150,200)){
PruebaTO<-Resultados[Resultados$k == k & Resultados$tipo=="p",] 
PruebaTP<-Resultados[Resultados$k == k & Resultados$tipo=="o",]

vecO<-PruebaTP$Tiempo
vecP<-PruebaTO$Tiempo
student<-t.test(vecO,vecP)
print(student)
}