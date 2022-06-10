########################################################
#                       FUNCIONES                      #
########################################################

ruido_muestra <- function(n, gamma, c){
  # Toma gamma entre 0 y 1, el tamaño de la muestra y el valor c
  # Devuelve la muestra con ruido de tamaño gamma y valor c
  # Si gamma = 0 devuelve una normal estandar de tamaño n
  muestra <- rnorm(n, 0, 1)
  if (gamma > 0) {
    porcentaje <- floor(n*gamma)
    ruido <- rep(c, porcentaje)
    muestra.nueva <- c(muestra, ruido)
    return(muestra.nueva)
  }
  else {
    return(muestra)
  }
}

test_empirico <- function(n, gamma, c){
  # Repite el muestreo 10000 veces y usa la funcion t.test() para rechazar
  # cuando p-valor < 0.05. Devuelve el porcentaje total de rechazos
  simulaciones = rep(0, 10000)
  for (k in 1:10000) {
    resultado <- t.test(ruido_muestra(n, gamma, c), mu=0)
    if (resultado$p.value < 0.05) {
      simulaciones[k] <- 1
    }
  }
  return(sum(simulaciones)/10000)
}

#################################################################
# Creo un data frame y le cargo los resultados con un doble for #
#################################################################

resultados <- data.frame(matrix(data=0, ncol = 6, nrow = 4))
colnames(resultados) <- c('c=1', 'c=3', 'c=6', 'c=10', 'c=15', 'c=20')
rownames(resultados) <- c('g = 0', 'g = 0.01', 'g = 0.05', 'g = 0.1')
ruido.c <- c(1, 3, 6, 10, 15, 20)
proporcion.g <- c(0, 0.01, 0.05, 0.1)

for (j in 1:6) {
  for (i in 1:4) {
    resultados[i, j] <- test_empirico(100, proporcion.g[i], ruido.c[j])
    cat('Resultado para c =', ruido.c[j], 'y gamma =', proporcion.g[i],
        ': ', resultados[i,j],  fill=TRUE)
  }
}
View(resultados)

#################################################################
# Calculos auxiliares, ploteos, pruebas y otras yerbas
#################################################################

# Box plot de los promedios de n repeticiones de una normal vs normal con ruido.
# Me armo una funcion a la que llamo con distintos valores de gamma
promedios_boxplot <- function(gamma) {
  n = 100
  i=1
  promedios <- data.frame(matrix(data=0, ncol=7, nrow=n))
  colnames(promedios) <- c('N(0,1)', 'C = 1', 'C = 3', 'C = 6', 'C = 10', 'C = 15', 'C = 20')
  while (i <= n){
    promedios[i, 1] <- mean(rnorm(101, 0, 1))
    promedios[i, 2] <- mean(ruido_muestra(100, gamma, 1))
    promedios[i, 3] <- mean(ruido_muestra(100, gamma, 3))
    promedios[i, 4] <- mean(ruido_muestra(100, gamma, 6))
    promedios[i, 5] <- mean(ruido_muestra(100, gamma, 10))
    promedios[i, 6] <- mean(ruido_muestra(100, gamma, 15))
    promedios[i, 7] <- mean(ruido_muestra(100, gamma, 20))
    i <- i + 1
  }
  boxplot(promedios, main=gamma)
}
par(mfrow=c(1,4))
promedios_boxplot(0)
promedios_boxplot(0.01)
promedios_boxplot(0.05)
promedios_boxplot(0.1)


