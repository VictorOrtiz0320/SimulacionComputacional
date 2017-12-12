library(ggplot2)
#Declaración del dataframe con las partículas iniciales
l<-3
velocidad<-l/30
tabla=data.frame()
datos=data.frame()
n <- 30 #Número de particulas 
matriz<-matrix(double(),nrow=n,ncol=n)
umbral<-0.05 #Distancia de umbral de interacción
dcc<-0.8 # Espesor de la doble capa electrica de interacción
tmax<-50 #duración de la simulación


Doblecapa<-function(pH){
  P1<-c(1,0.01)
  P2<-c(7,1)
  m<-(P2[2]-P1[2])/(P2[1]-P1[1])
  b=P2[2]-m*P2[1]
  
  e<-m*pH+b
    
    
    return(e)
}

for (pH in 1:7){
#Declaración del dataframe
  particulas<- data.frame(posicion=numeric(),x = double(), y = double(), dx = double(), dy = double(), espesor=double(), estado=character(), lider=numeric())
  for(i in 1:n){
    particulas <-rbind(particulas, data.frame(x = runif(1, 0, l), y=runif(1, 0, l), c =runif(1, 5, 5.1), r=5, dx=runif(1,-velocidad,velocidad), dy=runif(1,-velocidad,velocidad), espesor=runif(1,Doblecapa(pH),Doblecapa(pH+1)),estado="S",lider=0))
  }
  particulas$posicion<-seq(1,n,1)
  particulas$lider<-seq(1,n,1)
  levels(particulas$estado) <- c(levels(particulas$estado), "A","L")
  particulas$rf<-particulas$r+particulas$espesor

#Inicio de la simulación
for (tiempo in 1:tmax){
  
  for (i in seq(1,n-1,1)){
    p1<- particulas[i,]
    if (p1$estado != "A"){
      for (j in seq(i+1,n,1)){
        p2<-particulas[j,]
        if (p2$estado != "A"){
          if(i!=j){
          dx1<-p1$x-p2$x
          dy1<-p1$y-p2$y
          
            matriz[i,j]<-sqrt(dx1^2+dy1^2) #Calcular distancia euclidiana
            d<-matriz[i,j]
            #d<-d-particulas[i,]$espesor-particulas[j,]$espesor
            if (d<umbral) 
            {
              if (particulas[i,]$espesor<dcc && particulas[j,]$espesor<dcc){ #uniion
                
                if (particulas[j,]$estado=="L"){
                  
                  particulas[i,]$estado<-"A"
                  particulas[i,]$dx<-particulas[j,]$dx
                  particulas[i,]$dy<-particulas[j,]$dy
                  particulas[i,]$lider<-particulas[j,]$lider
                  particulas[j,]$estado<-"L"
                }
                else{
                  particulas[j,]$estado<-"A"
                  particulas[j,]$dx<-particulas[i,]$dx
                  particulas[j,]$dy<-particulas[i,]$dy
                  particulas[j,]$lider<-particulas[i,]$lider
                  particulas[i,]$estado<-"L"
                }
                print(paste("pos inicial particula",i, tiempo))
                print("aglomerado")
              }
              
              else{ 
                if(particulas[i,]$estado=="L"){
                  print(particulas[i,]$dx)
                  particulas[j,]$dx<-(particulas[j,]$dx*-1)
                  particulas[j,]$dy<-(particulas[j,]$dy*-1)
                  print(paste("pos inicial particula",i,j, tiempo))
                  print("repelen1")
                  print(particulas[i,]$dx)
                }else{
                  particulas[i,]$dx<-(particulas[i,]$dx*-1)
                  particulas[i,]$dy<-(particulas[i,]$dy*-1)
                  print("repelen2")
                  
                }
              }
              
            }
            
          }
          
        }
        
      }
    }
  }  
  
  #Movimiento Browniano
  for(i in 1:n){
    p<-particulas[i,]
    
    p$x<- p$x + p$dx #Movimiento en x
    p$y<- p$y + p$dy #Movimiento en y
    
    
    
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
  #tabla=rbind(tabla,particulas)

  #Graficar la simulación
  
  #ggplot()+
  ggplot(data=particulas, aes(x = particulas$x, y= particulas$y, label=particulas$estado)) +
    geom_point(data=particulas, aes(x = particulas$x, y= particulas$y,size=particulas$rf), color="gray80")+
    geom_point(data=particulas, aes(x = particulas$x, y= particulas$y),color="gray29")+
    geom_label(aes(fill = factor(particulas$lider)), colour = "white", fontface = "bold")+
    scale_x_continuous(name="x",limits = c(0, l))+
    scale_y_continuous(name="y",limits = c(0, l))+
    scale_colour_manual(values=colores)+  
    ggtitle(paste("Paso",tiempo, "pH", pH))+
    theme(plot.title = element_text(hjust = 0.5, size = 15))+
    guides(fill=FALSE, size=FALSE)+
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
  ggsave(paste("pH",pH,"Proyecto_p_",tiempo,".png"))
  
  #Grafico de bolitas
  ggplot()+
  #ggplot(data=particulas, aes(x = particulas$x, y= particulas$y, label=particulas$estado)) +
    geom_point(data=particulas, aes(x = particulas$x, y= particulas$y,size=particulas$rf), color="gray80")+
    geom_point(data=particulas, aes(x = particulas$x, y= particulas$y),color="gray29")+
    #geom_label(aes(fill = factor(particulas$lider)), colour = "white", fontface = "bold")+
    scale_x_continuous(name="x",limits = c(0, l))+
    scale_y_continuous(name="y",limits = c(0, l))+
    scale_colour_manual(values=colores)+  
    ggtitle(paste("Paso",tiempo, "pH", pH))+
    theme(plot.title = element_text(hjust = 0.5, size = 15))+
    guides(fill=FALSE, size=FALSE)+
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
  ggsave(paste("pH",pH,"Proyecto_bolitas_",tiempo,".png"))
  
  
  
  
  for (i in 1:n){
    res<-cbind(pH,tiempo,i,dim( particulas[particulas$lider==i,])[1])
    datos<-rbind(datos,res)
 
  
  }
  
  
  }
}
colnames(datos)<-c("pH","Tiempo","Lider","Tamaño")
datos$pH=as.factor(datos$pH)
#library(plyr)
#ggplot(ddply(datos, .(pH,Tiempo), mean), aes(x=factor(Tiempo), y=datos$Tamaño)) + 
 # geom_point()

  #ggsave("PuntosSimulados.png")

datos<-datos[datos$Tamaño!=0 & datos$Tiempo==tmax,  ]
#Histograma
ggplot(data=datos, aes(datos$Tamaño)) + 
 geom_histogram(bins = max(datos$Tamaño), color="white", fill="black")+
  facet_wrap(~pH)
ggsave("HistogramaS.png")


#Datos simulados



#Datos experimentales Tamaños vs pH

library(lattice)

testcsv<- read.csv(file="datos.csv", head=TRUE, sep = ",")
testcsv

names(testcsv)=c("Tiempo","Tamaño","pH")
testcsv$pH=as.factor(testcsv$pH)
xyplot(data=testcsv,Tamaño~Tiempo,groups = pH, xlab="Tiempo (s)", ylab="Tamaño (nm)",
       key=list(space="right",
                lines=list(col=c("deepskyblue2","hotpink1","green4","red"), lty=c(1,1), lwd=2),
                text=list(c("pH 1","pH 5","pH 6","pH 7"))
       ),
       panel=function(x,y,subscripts,groups){
         panel.grid(h=-1,v=-1)
         panel.xyplot(x,y)
         panel.stripplot(x,y,
                         groups = groups, subscripts = subscripts,pch=19,type="o")
       })
ggplot(data=testcsv, aes(testcsv$Tamaño, fill=factor(testcsv$pH))) + 
  geom_histogram(bins = 6, color="white")+
  facet_wrap(~pH)
ggsave("HistogramaE.png")


#Crear imagen gif

#library(magick)
#frames=lapply(1:tmax,function(x) image_read(paste("Proyecto_p_",x,".png")))
#animation <- image_animate(image_join(frames), fps=100)
#image_write(animation, paste("Proyecto", ".gif"))
