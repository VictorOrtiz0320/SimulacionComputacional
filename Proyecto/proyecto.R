library(ggplot2)
#Declaración del dataframe con las partículas iniciales
l<-1.5
velocidad<-l/60
n <- 30 #Número de particulas 
particulas<- data.frame(x = double(), y = double(), dx = double(), dy = double(), espesor=double())
for(i in 1:n){
particulas <-rbind(particulas, data.frame(x = runif(1, 0, l), y=runif(1, 0, l), c =-5, r=5, dx=runif(1,-velocidad,velocidad), dy=runif(1,-velocidad,velocidad), espesor=runif(1,0.01,1)))
}

particulas$rf<-particulas$r+particulas$espesor

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


  
#Graficar la simulación
ggplot() +
  geom_point(data=particulas, aes(x = particulas$x, y= particulas$y,size=particulas$rf))+
  geom_point(data=particulas, aes(x = particulas$x, y= particulas$y),color="blue")+
  scale_x_continuous(name="x",limits = c(0, 1.5))+
  scale_y_continuous(name="y",limits = c(0, 1.5))+
  scale_colour_manual(values=colores)+  
  ggtitle(paste("Paso",tiempo))+
  theme(plot.title = element_text(hjust = 0.5, size = 15))+
  #guides(color=guide_legend(title="Carga"))+
  theme(legend.title = element_text(colour="black", size=15))+
  theme(legend.text = element_text(colour="black", size = 15))+
  guides(size=guide_legend(title="Radio"))+
  theme(legend.title = element_text(colour="black", size=15))+
  theme(legend.text = element_text(colour="black", size = 15))+
  theme(axis.title.x = element_text(face="bold", vjust=-0.5, colour="black", size=18)) +
  theme(axis.title.y = element_text(face="bold", vjust=1.5, colour="black", size=18)) +
  theme(axis.text.x=element_text(size=18), axis.text.y=element_text(size=18))
ggsave(paste("Proyecto_p_",tiempo,".png"))
}

