
x=data.frame()
Resultados=data.frame()
for (i in 1:10){
  
  source('~/GitHub/SimulacionComputacional/P6/p6paralelizado/p6.R', encoding = 'UTF-8')
  
  source('~/GitHub/SimulacionComputacional/P6/original/original.R', encoding = 'UTF-8')
  Resultados=cbind(TiempoT,TiempoO) 
  x=rbind(x,Resultados)  
}

colnames(x)=c("Programa Paralelizado", "Programa Original")
png("Prac6.png",width=600, height=800,pointsize = 20)
boxplot(x,col=c("Blue","Red"),ylab="Tiempo de ejecución (s)")
graphics.off()
