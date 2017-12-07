library(ggplot2)
#Declaración del dataframe con las partículas iniciales
l<-3
velocidad<-l/60
n <- 80 #Número de particulas 
particulas<- data.frame(x = double(), y = double(), dx = double(), dy = double(), espesor=double(), estado=character(), lider=numeric())
for(i in 1:n){
particulas <-rbind(particulas, data.frame(x = runif(1, 0, l), y=runif(1, 0, l), c =-5, r=5, dx=runif(1,-velocidad,velocidad), dy=runif(1,-velocidad,velocidad), espesor=runif(1,0.01,0.1),estado="S",lider=0))
}
levels(particulas$estado) <- c(levels(particulas$estado), "A","L")
particulas$rf<-particulas$r+particulas$espesor
matriz<-matrix(double(),nrow=n,ncol=n)
#Movimiento Browniano
tmax<-50
for (tiempo in 1:tmax){
  for(i in 1:n){
    p<-particulas[i,]
    
    p$x<- p$x + p$dx
    p$y<- p$y + p$dy
    
    if (p$x > l) {
      p$x <- p$x - l
    }
    if (p$y > l) {
      p$y <- p$y - l
    }
    if (p$x < 0) {
      p$x <- p$x + l
    }
    if (p$y < 0) {
      p$y <- p$y + l
    }
    particulas[i,]<-p
  }
  
umbral<-0.06


#Calcular distancia euclidiana

 for (i in 1:n){
   p1<- particulas[i,]
   if (p1$estado != "A"){
     for (j in 1:n){
       p2<-particulas[j,]
       dx<-p1$x-p2$x
       dy<-p1$y-p2$y
       if(i!=j){
         matriz[i,j]<-sqrt(dx^2+dy^2)
         d<-matriz[i,j]
         if (d<umbral)
         {
           particulas[j,]$estado<-"L"
           particulas[i,]$estado<-"A"
           particulas[i,]$dx<-particulas[j,]$dx
           particulas[i,]$dy<-particulas[j,]$dy
         }
       }
     }
   }   
 }
#Determinar la densidad de la doble capa

#FuerzaAtracción->function(){}#Calcular fuerzas de atracción de van der waals 
#Función inversa de la distancia pero elevada a un exponente mayor de dos

#FuerzaRepulsión->function(){}#Calcular fuerzas de repulsión electrostatica
#Fuerza de repulsión es inversamente proporcional al cuadrado de la distancia 


#Establecer una distancia de umbral de interacción

  
#Graficar la simulación
ggplot() +
  geom_point(data=particulas, aes(x = particulas$x, y= particulas$y,size=particulas$rf), color="gray80")+
  geom_point(data=particulas, aes(x = particulas$x, y= particulas$y),color="gray29")+
  scale_x_continuous(name="x",limits = c(0, 3))+
  scale_y_continuous(name="y",limits = c(0, 3))+
  scale_colour_manual(values=colores)+  
  ggtitle(paste("Paso",tiempo))+
  theme(plot.title = element_text(hjust = 0.5, size = 15))+
  #guides(color=guide_legend(title="Carga"))+
  theme(legend.title = element_text(colour="black", size=15))+
  theme(legend.text = element_text(colour="black", size = 15))+
  #guides(size=guide_legend(title="Radio"))+
  theme(legend.title = element_text(colour="black", size=15))+
  theme(legend.text = element_text(colour="black", size = 15))+
  theme(axis.title.x = element_text(face="bold", vjust=-0.5, colour="black", size=18)) +
  theme(axis.title.y = element_text(face="bold", vjust=1.5, colour="black", size=18)) +
  theme(axis.text.x=element_text(size=18), axis.text.y=element_text(size=18))+
  theme_bw() + 
  theme(
    panel.background = element_rect(fill = "aliceblue"),
    panel.grid.minor = element_line(linetype = "dotted")
  )
ggsave(paste("Proyecto_p_",tiempo,".png"))
}

