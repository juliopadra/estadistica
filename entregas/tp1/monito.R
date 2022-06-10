monito <- function(palabra){
  # Esta funcion toma vectores de 0 y 1 y devuelve la cantidad de intentos que necesita
  # el mono para escribirla tipeando de manera aleatoria
  n <- length(palabra)
  escrito <- sample(x = c(0, 1), size = n, replace = TRUE)
  k <- n
  while (identical(escrito, palabra) == FALSE) {
    escrito <- escrito[2:n]
    nuevaLetra <- sample(x = c(0, 1), size = 1)
    escrito <- c(escrito, nuevaLetra)
    k <- k + 1
  }
  return(k)
}

# Armo un data frame para guardar los distintos resultados de cada palabra
palabras <- c('AA', 'AB', 'AAA', 'AAB', 'ABA', 'BABA', 'BABAB')
intentos <- data.frame(matrix(ncol = 7, nrow = 1000))
colnames(intentos) <- palabras


for (i in 1:1000){
  # Este ciclo calcula el tiempo que tarda el mono. Cambiando las entradas 
  # de la funcion monito() busco los tiempos para las distintas palabras pedidas.
  intentos$AA[i] <- monito(c(0,0))
  intentos$AB[i] <- monito(c(0,1))
  intentos$AAA[i] <- monito(c(0,0,0))
  intentos$AAB[i] <- monito(c(0,0,1))
  intentos$ABA[i] <- monito(c(0,1,0))
  intentos$BABA[i] <- monito(c(1,0,1,0))
  intentos$BABAB[i] <- monito(c(1,0,1,0,1))
}

# Promedio de intentos por palabra (1000 repeticiones)
intentosPromedio <- c(mean(intentos$AA),mean(intentos$AB),mean(intentos$AAA),mean(intentos$AAB),
                      mean(intentos$ABA),mean(intentos$BABA),mean(intentos$BABAB))


# Plots #
barplot(intentosPromedio, main='Intentos promedio por palabra',
        names.arg=c('AA','AB','AAA','AAB','ABA','BABA','BABAB'),
        col='darkmagenta')

hist(intentos$BABAB, main='Histograma de intentos', xlab='BABAB', col='darkmagenta')
hist(intentos$BABA, main='Histograma de intentos', xlab='BABA', col='darkmagenta')
hist(intentos$ABA, main='Histograma de intentos', xlab='ABA', col='darkmagenta')
hist(intentos$AAA, main='Histograma de intentos', xlab='AAA', col='darkmagenta')
hist(intentos$AB, main='Histograma de intentos', xlab='AB', col='darkmagenta')
hist(intentos$AA, main='Histograma de intentos', xlab='AA', col='darkmagenta')
