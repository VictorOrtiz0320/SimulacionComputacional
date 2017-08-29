primo <- function(n) {
    if (n < 4) {
       return(TRUE)
    }
    for (i in 2:(n-1)) {
        if (n > i && (n %% i) == 0) { # residuo es cero
            return(FALSE)
        }
    }
    return(TRUE)
}
 
resultados <- numeric() # un vector vacio
for (n in 1:5) {
    resultados <-  c(resultados, primo(n)) # combinar vectores
}
cat(resultados, "\n")

