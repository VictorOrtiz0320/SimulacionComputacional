
x=data.frame()
Resultados=data.frame()
for (i in 1:4){
  
  source('~/GitHub/SimulacionComputacional/P6/p6paralelizado/p6.R', encoding = 'UTF-8')
  
  source('~/GitHub/SimulacionComputacional/P6/original/original.R', encoding = 'UTF-8')
  Resultados=cbind(TiempoT,TiempoO) 
  x=rbind(x,Resultados)  
}

colnames(x)=c("Programa Paralelizado", "Programa Original")
boxplot(x,cols="Blue", "Red",ylab="Tiempo de ejecución")
