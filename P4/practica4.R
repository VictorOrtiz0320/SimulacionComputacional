
# Función para inicializar la dirección de la grieta
inicio <- function() {
  direccion <- sample(1:4, 1)
  xg <- NULL
  yg <- NULL
  if (direccion == 1) { # vertical
    xg <- 1
    yg <- sample(1:n, 1)
  } else if (direccion == 2) { # horiz izr -> der
    xg <- sample(1:n, 1)
    yg <- 1
  } else if (direccion == 3) { # horiz der -> izq
    xg <- n
    yg <- sample(1:n, 1)
  } else { # vertical al reves
    xg <- sample(1:n, 1)
    yg <- n
  }
  return(c(xg, yg))
}
#Evaluar la posición de los vecinos de una semilla
vp <- data.frame(numeric(), numeric()) # posiciones de posibles vecinos
for (dx in -1:1) {
  for (dy in -1:1) {
    if (dx != 0 | dy != 0) { # descartar la posicion misma
      vp <- rbind(vp, c(dx, dy))
    }
  }
}
names(vp) <- c("dx", "dy")
vc <- dim(vp)[1]

celda <-  function(pos) {
  fila <- floor((pos - 1) / n) + 1
  columna <- ((pos - 1) %% n) + 1
  if (zona[fila, columna] > 0) { # es una semilla
    return(zona[fila, columna])
  } else {
    cercano <- NULL # sin valor por el momento
    menor <- n * sqrt(2) # mayor posible para comenzar la busqueda
    for (semilla in 1:k) {
      dx <- columna - x[semilla]
      dy <- fila - y[semilla]
      dist <- sqrt(dx^2 + dy^2)
      if (dist < menor) {
        cercano <- semilla
        menor <- dist
      }
    }
    return(cercano)
  }
}

suppressMessages(library(doParallel))
registerDoParallel(makeCluster(detectCores() - 1))
resultados=data.frame()

for (size in c(20, 40, 60, 80, 100, 120)) {
  
  for (k in c(10,50, 100)){
  
    #Generar el paisaje y acomodar las semillas
    n <-  size
    zona <- matrix(rep(0, n * n), nrow = n, ncol = n)
    x <- rep(0, k) # ocupamos almacenar las coordenadas x de las semillas
    y <- rep(0, k) # igual como las coordenadas y de las semillas

    for (semilla in 1:k) {
      while (TRUE) { # hasta que hallamos una posicion vacia para la semilla
        fila <- sample(1:n, 1)
        columna <- sample(1:n, 1)
        if (zona[fila, columna] == 0) {
          zona[fila, columna] = semilla
          x[semilla] <- columna
          y[semilla] <- fila
          break
        }
      }
    }

    celdas <- foreach(p = 1:(n * n), .combine=c) %dopar% celda(p)
    voronoi <- matrix(celdas, nrow = n, ncol = n, byrow=TRUE)
    rotate <- function(x) t(apply(x, 2, rev))
    #png("p4s.png")
    #par(mar = c(0,0,0,0))
    #image(rotate(zona), col=rainbow(k+1), xaxt='n', yaxt='n')
    #graphics.off()
    #png("p4c.png")
    #par(mar = c(0,0,0,0))
    #image(rotate(voronoi), col=rainbow(k+1), xaxt='n', yaxt='n')
    #graphics.off()

    limite <- n # grietas de que largo minimo queremos graficar

    propaga <- function(replica) {
    # probabilidad de propagacion interna
    prob <- 1
    dificil <- 0.99
    grieta <- voronoi # marcamos la grieta en una copia
    i <- inicio() # posicion inicial al azar
    xg <- i[1]
    yg <- i[2]
    largo <- 0
    while (TRUE) { # hasta que la propagacion termine
      grieta[yg, xg] <- 0 # usamos el cero para marcar la grieta
      largo <-  largo + 1
      frontera <- numeric()
      interior <- numeric()
      for (v in 1:vc) {
        vecino <- vp[v,]
        xs <- xg + vecino$dx # columna del vecino potencial
        ys <- yg + vecino$dy # fila del vecino potencial
        if (xs > 0 & xs <= n & ys > 0 & ys <= n) { # no sale de la zona
          if (grieta[ys, xs] > 0) { # aun no hay grieta ahi
            if (voronoi[yg, xg] == voronoi[ys, xs]) {
              interior <- c(interior, v)
            } else { # frontera
              frontera <- c(frontera, v)
            }
          }
        }
      }
      elegido <- 0
      if (length(frontera) > 0) { # siempre tomamos frontera cuando haya
        if (length(frontera) > 1) {
          elegido <- sample(frontera, 1)
        } else {
          elegido <- frontera # sample sirve con un solo elemento
        }
        prob <- 1 # estamos nuevamente en la frontera
      } else if (length(interior) > 0) { # no hubo frontera para propagar
        if (runif(1) < prob) { # intentamos en el interior
          if (length(interior) > 1) {
            elegido <- sample(interior, 1)
          } else {
            elegido <- interior
          }
          prob <- dificil * prob # mas dificil a la siguiente
        }
      }
      if (elegido > 0) { # si se va a propagar
        vecino <- vp[elegido,]
        xg <- xg + vecino$dx
        yg <- yg + vecino$dy
      } else {
        break # ya no se propaga
      }
    }
    
    #if (largo >= limite) {
     # png(paste("p4g_", replica, ".png", sep=""))
      #par(mar = c(0,0,0,0))
      #image(rotate(grieta), col=rainbow(k+1), xaxt='n', yaxt='n')
      #graphics.off()
   # }
    return(largo)
  }

  stopImplicitCluster()
  largos <- foreach(r = 1:100, .combine=c) %dopar% propaga(r)
  resultados=rbind(resultados,largos)
  }
}


t.resultado=t(resultados) #matriz transpuesta 
colnames(t.resultado)=c(10,50,100,10,50,100,10,50,100,10,50,100,10,50,100,10,50,100)
g1=subset.data.frame(t.resultado, select =c(1,2,3 ))
g2=subset.data.frame(t.resultado, select =c(4,5,6 ))
g3=subset.data.frame(t.resultado, select =c(7,8,9 ))
g4=subset.data.frame(t.resultado, select =c(10,11,12))
g5=subset.data.frame(t.resultado, select =c(13,14,15))
g6=subset.data.frame(t.resultado, select =c(16,17,18))
png("screen.png",width = 1200,height = 1800, pointsize = 20)
  split.screen(c(3,1))
  split.screen(c(1,2), screen = 1)
  split.screen(c(1,2), screen = 2)
  split.screen(c(1,2), screen = 3)
  screen(4)
    boxplot(g1,col=(c("yellow","orange","red")),xlab="Semillas", ylab="Largo de la grieta",ylim=c(0,300))
  screen(5)
    boxplot(g2,col=(c("yellow","orange","red")),xlab="Semillas", ylab="Largo de la grieta",ylim=c(0,300))
  screen(6)
    boxplot(g3,col=(c("yellow","orange","red")),xlab="Semillas", ylab="Largo de la grieta",ylim=c(0,300))
  screen(7)
    boxplot(g4,col=(c("yellow","orange","red")),xlab="Semillas", ylab="Largo de la grieta",ylim=c(0,300))
  screen(8)
    boxplot(g5,col=(c("yellow","orange","red")),xlab="Semillas", ylab="Largo de la grieta",ylim=c(0,150))
  screen(9)
    boxplot(g6,col=(c("yellow","orange","red")),xlab="Semillas", ylab="Largo de la grieta",ylim=c(0,150))
dev.off()

