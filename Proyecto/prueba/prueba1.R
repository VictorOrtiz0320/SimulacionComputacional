library(ggplot2)
#Declaraci�n del dataframe con las part�culas iniciales
l<-3
velocidad<-l/30
tabla=data.frame()
datos=data.frame()
n <- 50 #N�mero de particulas 
particulas<- data.frame(posicion=numeric(),x = double(), y = double(), dx = double(), dy = double(), espesor=double(), estado=character(), lider=numeric())
for(i in 1:n){
  particulas <-rbind(particulas, data.frame(x = runif(1, 0, l), y=runif(1, 0, l), c =runif(1, 5, 5.1), r=5, dx=runif(1,-velocidad,velocidad), dy=runif(1,-velocidad,velocidad), espesor=runif(1,0.01,0.1),estado="S",lider=0))
}
particulas$posicion<-seq(1,n,1)
particulas$lider<-seq(1,n,1)
levels(particulas$estado) <- c(levels(particulas$estado), "A","L")
particulas$rf<-particulas$r+particulas$espesor
matriz<-matrix(double(),nrow=n,ncol=n)
umbral<-0.02 #Distancia de umbral de interacci�n
dcc<-0.05 # umbral de espesor de la doble capa electrica de interacci�n


tmax<-50 #duraci�n de la simulaci�n

#for (pH in 1:7){}
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
            #matriz[i,j]<-sqrt(dx1^2+dy1^2) 
            #d<-matriz[i,j]
            d<-sqrt(dx1^2+dy1^2)#Calcular distancia euclidiana
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
              
              else{ #if (particulas[i,]$espesor>dcc | particulas[j,]$espesor>dcc){ #repelen
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
  tabla=rbind(tabla,particulas)
  #FuerzaAtracci�n->function(){}#Calcular fuerzas de atracci�n de van der waals 
  #Funci�n inversa de la distancia pero elevada a un exponente mayor de dos
  #A=0.1*10^-20 #constante de Hamaker
  #Va=(-(A*(particulas[i,]$rf/(6*d))))
  #Va=(particulas[i,]$c*particulas[j,]$c)/d^6
  
  #FuerzaRepulsi�n->function(){}#Calcular fuerzas de repulsi�n electrostatica
  #Fuerza de repulsi�n es inversamente proporcional al cuadrado de la distancia 
  #Vr=(particulas[i,]$c*particulas[j,]$c)/d^2
  #Fuerza de DLVO
  
  # Vt=Va-Vr
  
  
  
  #Graficar la simulaci�n
  
  #ggplot()+
  ggplot(data=particulas, aes(x = particulas$x, y= particulas$y, label=particulas$estado)) +
    geom_point(data=particulas, aes(x = particulas$x, y= particulas$y,size=particulas$rf), color="gray80")+
    geom_point(data=particulas, aes(x = particulas$x, y= particulas$y),color="gray29")+
    geom_label(aes(fill = factor(particulas$lider)), colour = "white", fontface = "bold")+
    scale_x_continuous(name="x",limits = c(0, l))+
    scale_y_continuous(name="y",limits = c(0, l))+
    scale_colour_manual(values=colores)+  
    ggtitle(paste("Paso",tiempo))+
    theme(plot.title = element_text(hjust = 0.5, size = 15))+
    guides(fill=FALSE, size=FALSE)+
    #theme(legend.title = element_text(colour="black", size=15))+
    #theme(legend.text = element_text(colour="black", size = 15))+
    #guides(size=guide_legend(title="Radio"))+
    #theme(legend.title = element_text(colour="black", size=15))+
    #theme(legend.text = element_text(colour="black", size = 15))+
    #theme(axis.title.x = element_text(face="bold", vjust=-0.5, colour="black", size=18)) +
    #theme(axis.title.y = element_text(face="bold", vjust=1.5, colour="black", size=18)) +
    #theme(axis.text.x=element_text(size=18), axis.text.y=element_text(size=18))+
    theme_bw() + 
    theme(
      panel.background = element_rect(fill = "aliceblue"),
      panel.grid.minor = element_line(linetype = "dotted")
    )
  ggsave(paste("Proyecto_p_",tiempo,".png"))
  
  
  for (i in 1:n){
    res<-cbind(tiempo,i,dim( particulas[particulas$lider==i,])[1])
    datos<-rbind(datos,res)
    
  }
  
}


#Datos simulados


#Grafico de puntos
colnames(datos)<-c("Tiempo","Lider","Tama�o")
datos$Tiempo=as.factor(datos$Tiempo)
ggplot(data=datos,aes(x=Tiempo,y=Tama�o))+
  geom_point()



#Histograma
datos<-datos[datos$Tama�o!=0 & datos$Tiempo==50,  ]
ggplot(data=datos, aes(datos$Tama�o)) + 
  geom_histogram(bins = max(datos$Tama�o), color="white", fill="black")






#Datos experimentales Tama�os vs pH

library(lattice)

testcsv<- read.csv(file="datos.csv", head=TRUE, sep = ",")
testcsv

names(testcsv)=c("Tiempo","Tama�o","pH")
testcsv$pH=as.factor(testcsv$pH)
xyplot(data=testcsv,Tama�o~Tiempo,groups = pH, xlab="Tiempo (s)", ylab="Tama�o (nm)",
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



#Crear imagen gif

#library(magick)
#frames=lapply(1:tmax,function(x) image_read(paste("Proyecto_p_",x,".png")))
#animation <- image_animate(image_join(frames), fps=100)
#image_write(animation, paste("Proyecto", ".gif"))
