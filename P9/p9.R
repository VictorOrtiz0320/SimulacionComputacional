n <- 50
p <- data.frame(x = rnorm(n), y=rnorm(n), c=rnorm(n), m=abs(rnorm(n,0.001,0.1)),r=0)
xmax <- max(p$x)
xmin <- min(p$x)
p$x <- (p$x - xmin) / (xmax - xmin) # ahora son de 0 a 1
ymax <- max(p$y)
ymin <- min(p$y)
p$y <- (p$y - ymin) / (ymax - ymin) # las y tambien
cmax <- max(p$c)
cmin <- min(p$c)
p$c <- 2 * (p$c - cmin) / (cmax - cmin) - 1 # cargas son entre -1 y 1
p$g <- round(5 * p$c) # coloreamos segun la carga a 11 niveles de -5 a 5
p$r=p$m
paso <- floor(256 / 10)
niveles <- seq(0, 255, paso)
colores <- rgb(niveles, rep(0, 11), rev(niveles), max=255)
png("p9i.png")
library(lattice)
xyplot(y ~ x, group=g, data=p, auto.key=list(space="right"),
       xlab="X", ylab="Y", main="Part\u{00ed}culas generadas",
       par.settings = list(superpose.symbol = list(pch = 15, cex = 1.5,
                                                   col = colores)))
pix=p$x
piy=p$y

resul=data.frame()
final=data.frame()
graphics.off()
eps <- 0.001
fuerza <- function(i) {
  xi <- p[i,]$x
  yi <- p[i,]$y
  ci <- p[i,]$c
  mi <- p[i,]$m
  fx <- 0
  fy <- 0
  for (j in 1:n) {
    cj <- p[j,]$c
    dir <- (-1)^(1 + 1 * (ci * cj < 0))
    dx <- (xi - p[j,]$x)
    dy <- (yi - p[j,]$y)
    factor <- dir * abs(ci - cj) / (sqrt(dx^2 + dy^2) + eps)
    fx <- fx - dx*factor
    fy <- fy - dy*factor

  }
  return(c(fx, fy))
}

suppressMessages(library(doParallel))
registerDoParallel(makeCluster(detectCores() - 1))
#system("rm -f p9_t*.png") # borramos anteriores en el caso que lo hayamos corrido
tmax <- 100
digitos <- floor(log(tmax, 10)) + 1
tl <- "0"
while (nchar(tl) < digitos) {
  tl <- paste("0", tl, sep="")
}
png(paste("p9_t", tl, ".png", sep=""))
plot(p$x, p$y, col=colores[p$g+6], pch=15, cex=1.5, xlim=c(-0.1, 1.1), ylim=c(-0.1, 1.1),
     main="Estado inicial", xlab="X", ylab="Y")
graphics.off()
p$g=as.factor(p$g)

for (iter in 1:tmax) {
  f <- foreach(i = 1:n, .combine=c) %dopar% fuerza(i)
  delta <- 0.0008 / max(abs(f)) # que nadie desplace una paso muy largo
  p$x <- foreach(i = 1:n, .combine=c) %dopar% max(min(p[i,]$x + delta * (f[c(TRUE, FALSE)][i])/p[i,]$m, 1), 0)
  p$y <- foreach(i = 1:n, .combine=c) %dopar% max(min(p[i,]$y + delta * (f[c(FALSE, TRUE)][i])/p[i,]$m, 1), 0)
  p$v<- foreach (i = 1:n, .combine=c) %dopar% (delta*(sqrt(((f[c(TRUE, FALSE)][i])/p[i,]$m)^2+((f[c(FALSE, TRUE)][i])/p[i,]$m)^2)))
  tl <- paste(iter, "", sep="")
  while (nchar(tl) < digitos) {
    tl <- paste("0", tl, sep="")
  }
  
library(ggplot2)  
  ggplot() +
    geom_point(data=p, aes(x = p$x, y= p$y,size=p$r,color=p$g))+
    scale_x_continuous(name="x",limits = c(0, 1))+
    scale_y_continuous(name="y",limits = c(0, 1))+
    scale_colour_manual(values=colores)+  
    ggtitle(paste("Paso",iter))+
    theme(plot.title = element_text(hjust = 0.5, size = 15))+
    guides(color=guide_legend(title="Carga"))+
    theme(legend.title = element_text(colour="black", size=15))+
    theme(legend.text = element_text(colour="black", size = 15))+
    guides(size=guide_legend(title="Radio"))+
    theme(legend.title = element_text(colour="black", size=15))+
      theme(legend.text = element_text(colour="black", size = 15))+
    theme(axis.title.x = element_text(face="bold", vjust=-0.5, colour="black", size=18)) +
    theme(axis.title.y = element_text(face="bold", vjust=1.5, colour="black", size=18)) +
    theme(axis.text.x=element_text(size=18), axis.text.y=element_text(size=18))
  
 ggsave(paste("P9_p_",iter,".png"))
  
}
stopImplicitCluster()

ggplot(data=p, aes(x=p$m, y=p$v))+
  xlab("Masa")+ylab("Velocidad")+
  geom_point()+
  geom_smooth(method = "lm",formula=y~log(x),se=FALSE)
ggsave(paste("Graifco1.png"))
regresion<-lm(data=p, p$v~log(p$m))
summary(regresion)

library(magick)
frames=lapply(1:tmax,function(x) image_read(paste("P9_p_",x,".png")))
animation <- image_animate(image_join(frames), fps=100)
image_write(animation, paste("P9_R1", ".gif"))

#system("convert -delay 50 -size 300x300 p9_t*.png -loop 0 p9.gif") # creamos animacion con ImageMagick
#Si la distancia euclideana es mejo	r a la media de los radios se van a sobreponer
#impares<- seq(1,dim(final)[1],2)
#Nparticula<-data.frame(x=final[impares,])

