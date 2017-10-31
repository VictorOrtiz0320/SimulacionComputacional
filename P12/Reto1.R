suppressMessages(library(doParallel))
registerDoParallel(makeCluster(detectCores() - 1))
Resultados=data.frame()
Toriginal=numeric()
Tparalelo=numeric()
a=200
for (negro in seq(0.09,0.99,0.1))
  for (gris in seq(0.09,0.99,0.1))
    for (blanco in seq(0.09,0.99,0.1)){
    
    source('~/GitHub/SimulacionComputacional/P12/P12p.R')
    Tparalelo=cbind(a,"p",AciertosP,negro,gris,blanco)
    print(Tparalelo)
    Resultados=rbind(Resultados,Tparalelo)
}
stopImplicitCluster() 
names(Resultados)=c("nPrueba","tipo","Aciertos","negro","gris","blanco")
Resultados$Aciertos<-as.numeric(levels(Resultados$Paciertos))[Resultados$Aciertos]
Resultados$gris<-as.numeric(levels(Resultados$gris))[Resultados$gris]
Resultados$negro<-as.numeric(levels(Resultados$negro))[Resultados$negro]
Resultados$blanco<-as.numeric(levels(Resultados$blanco))[Resultados$blanco]
#Resultados$tipo=as.factor(Resultados$tipo)

library(ggplot2)

split.screen(c(3,1))
split.screen(c(1,2), screen = 1)
screen(1)
ggplot(Resultados, aes(negro, blanco)) + 
  geom_raster(aes(fill=Aciertos)) +
  scale_fill_gradient(low="dodgerblue", high="dodgerblue4")+
  theme(legend.title = element_text(colour="black", size=15))+
  theme(legend.text = element_text(colour="black", size = 15))+
  theme(axis.title.x = element_text(face="bold", colour="black", size=18)) +
  theme(axis.title.y = element_text(face="bold", colour="black", size=18)) +
  theme(axis.text.x=element_text(size=18), axis.text.y=element_text(size=18))+
  coord_fixed(ratio = 1)  
ggsave(filename = "n-b.png")
screen(2)
ggplot(Resultados, aes(gris, blanco)) + 
  geom_raster(aes(fill=Aciertos)) +
  scale_fill_gradient(low="dodgerblue", high="dodgerblue4")+
  theme(legend.title = element_text(colour="black", size=15))+
  theme(legend.text = element_text(colour="black", size = 15))+
  theme(axis.title.x = element_text(face="bold", colour="black", size=18)) +
  theme(axis.title.y = element_text(face="bold", colour="black", size=18)) +
  theme(axis.text.x=element_text(size=18), axis.text.y=element_text(size=18))+
  coord_fixed(ratio = 1)  
ggsave(filename = "g-b.png")
screen(3)
ggplot(Resultados, aes(gris, negro)) + 
  geom_raster(aes(fill=Aciertos)) +
  scale_fill_gradient(low="dodgerblue", high="dodgerblue4")+
  theme(legend.title = element_text(colour="black", size=15))+
  theme(legend.text = element_text(colour="black", size = 15))+
  theme(axis.title.x = element_text(face="bold", colour="black", size=18)) +
  theme(axis.title.y = element_text(face="bold", colour="black", size=18)) +
  theme(axis.text.x=element_text(size=18), axis.text.y=element_text(size=18))+
  coord_fixed(ratio = 1)  
ggsave(filename = "g-n.png")
dev.off()
save.image(file = "Practica11chido.RData")