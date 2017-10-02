x=data.frame()
Resultados=data.frame()
for (i in 1:5){
  
  source('~/GitHub/SimulacionComputacional/P8/p8original/p8original.R', encoding = 'UTF-8')
  source('~/GitHub/SimulacionComputacional/P8/p8para/parap8.R')

  Resultados=cbind(TiempoO,TiempoT) 
  x=rbind(x,Resultados)  
}

colnames(x)=c("Programa Paralelizado", "Programa Original")
#png("Prac8tiempos.png",width=600, height=800,pointsize = 20)
boxplot(x,col=c("Blue","Red"))
#graphics.off()
