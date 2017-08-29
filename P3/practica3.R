
primo <- function(n) {
    if (n == 1 || n == 2) {
        return(TRUE)
    }
    if (n %% 2 == 0) {
        return(FALSE)
    }
    for (i in seq(3, max(3, ceiling(sqrt(n))), 2)) {
        if ((n %% i) == 0) {
            return(FALSE)
        }
    }
    return(TRUE)
}

#Hacer un vector de puro numeros primos y mandarlos directamente
suppressMessages(library(doParallel))
datos=data.frame()

desde <- 1000
hasta <-  10000
original <- desde:hasta
invertido <- hasta:desde
pares = seq(desde+1, (desde + 1 + 2*(hasta-desde))/2,2)
replicas <- 20
MaxNucleos=detectCores()-1


for (nucleos in 1:MaxNucleos){
 
  registerDoParallel(makeCluster(nucleos))
  ot <-  numeric()
  it <-  numeric()
  at <-  numeric()
  tp <-  numeric()
  
  for (r in 1:replicas) {
    ot <- c(ot, system.time(foreach(n = original, .combine=c) %dopar% primo(n))[3]) # de menor a mayor
    it <- c(it, system.time(foreach(n = invertido, .combine=c) %dopar% primo(n))[3]) # de mayor a menor
    at <- c(at, system.time(foreach(n = sample(original), .combine=c) %dopar% primo(n))[3]) # orden aleatorio
    tp=   c(tp, system.time(foreach(n = pares  , .combine=c) %dopar% primo(n))[3]) #números pares
  }
  stopImplicitCluster()

  
  datos=rbind(ot,it,at,tp)
 
  
 #Generar archivos de imagen de los gráficos
  
  nArchivo = paste("p2_t", nucleos, ".png", sep="")
  NumeroNucleo = paste("Núcleos usados:", nucleos)
  png(nArchivo)
  boxplot(data.matrix(datos),use.cols=FALSE, xlab="Tipos de ordenamiento", ylab="Tejeución", main=NumeroNucleo)
  graphics.off()

}


