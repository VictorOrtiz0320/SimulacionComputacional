library(ggplot2)
#Declaración del dataframe con las partículas iniciales
l<-3
velocidad<-l/60
tabla=data.frame()
n <- 20 #Número de particulas 
particulas<- data.frame(posicion=numeric(),x = double(), y = double(), dx = double(), dy = double(), espesor=double(), estado=character(), lider=numeric())
for(i in 1:n){
particulas <-rbind(particulas, data.frame(x = runif(1, 0, l), y=runif(1, 0, l), c =runif(1, 5, 5.1), r=5, dx=runif(1,-velocidad,velocidad), dy=runif(1,-velocidad,velocidad), espesor=runif(1,0.01,0.1),estado="S",lider=0))
}
particulas$posicion<-seq(1,n,1)
particulas$lider<-seq(1,n,1)
levels(particulas$estado) <- c(levels(particulas$estado), "A","L")
particulas$rf<-particulas$r+particulas$espesor
matriz<-matrix(double(),nrow=n,ncol=n)
umbral<-0.02 #Distancia de umbral de interacción
dcc<-0.05 # Espesor de la doble capa electrica de interacción


tmax<-80 #duración de la simulación


for (tiempo in 1:tmax){


 for (i in seq(1,n-1,1)){
  
   p1<- particulas[i,]
   if (p1$estado != "A"){
     
     for (j in seq(i+1,n,1)){
       p2<-particulas[j,]
       if (p2$estado != "A"){
       dx1<-p1$x-p2$x
       dy1<-p1$y-p2$y
       if(i!=j){
         matriz[i,j]<-sqrt(dx1^2+dy1^2) #Calcular distancia euclidiana
         d<-matriz[i,j]
         d<-d-particulas[i,]$espesor-particulas[j,]$espesor
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
           
          else if (particulas[i,]$espesor>dcc | particulas[j,]$espesor>dcc){ #repelen
            print(particulas[i,]$dx)
            particulas[i,]$dx<-particulas[i,]$dx*-1
            particulas[i,]$dy<-particulas[i,]$dy*-1
            particulas[j,]$dx<-particulas[j,]$dx*-1
            particulas[j,]$dy<-particulas[j,]$dy*-1
            print(paste("pos inicial particula",i,j, tiempo))
            print("repelen")
            print(particulas[i,]$dx)
          
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
tabla=rbind(tabla,particulas)
#FuerzaAtracción->function(){}#Calcular fuerzas de atracción de van der waals 
#Función inversa de la distancia pero elevada a un exponente mayor de dos
  #A=0.1*10^-20 #constante de Hamaker
  #Va=(-(A*(particulas[i,]$rf/(6*d))))
  #Va=(particulas[i,]$c*particulas[j,]$c)/d^6
  
#FuerzaRepulsión->function(){}#Calcular fuerzas de repulsión electrostatica
#Fuerza de repulsión es inversamente proporcional al cuadrado de la distancia 
  #Vr=(particulas[i,]$c*particulas[j,]$c)/d^2
#Fuerza de DLVO
    
   # Vt=Va-Vr


  
#Graficar la simulación

#ggplot()+
ggplot(data=particulas, aes(x = particulas$x, y= particulas$y, label=rownames(particulas))) +
  geom_point(data=particulas, aes(x = particulas$x, y= particulas$y,size=particulas$rf), color="gray80")+
  geom_point(data=particulas, aes(x = particulas$x, y= particulas$y),color="gray29")+
  geom_label(aes(fill = factor(particulas$lider)), colour = "white", fontface = "bold")+
  scale_x_continuous(name="x",limits = c(0, l))+
  scale_y_continuous(name="y",limits = c(0, l))+
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
         #panel.abline(h=wolfram,col="Green",lwd=2)
       })

#Crear imagen gif

#library(magick)
#frames=lapply(1:tmax,function(x) image_read(paste("Proyecto_p_",x,".png")))
#animation <- image_animate(image_join(frames), fps=100)
#image_write(animation, paste("Proyecto", ".gif"))
