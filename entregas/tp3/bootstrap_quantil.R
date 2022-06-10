# Cargamos los datos del dataset Titanic
library(readr)
datos_titanic <- read_csv("Documents/estadistica/prac/datos_titanic.csv")
data = datos_titanic$Fare

# Obtengo un vector de nboot realizaciones de la mediana muestral
obtener.realizaciones = function(data, q){
  realizaciones = rep(NA,500)
  for(i in 1:500){
    muestra.nueva = sample(x = data, size = length(data), replace = T)
    realizaciones[i] = quantile(muestra.nueva, q)
  }
  return(realizaciones)
}


estimar.varianza.boot = function(data, q){
  realizaciones = obtener.realizaciones(data, q)
  return(var(realizaciones))
}

met.1 = function(data, alfa = 0.05, q){
  centro = quantile(data, q)
  varianza = estimar.varianza.boot(data, q)
  return(list(inf = centro - qnorm(1 - alfa/2) * sqrt(varianza),
              sup = centro + qnorm(1 - alfa/2) * sqrt(varianza)))
}

met.2 = function(data, alfa = 0.05){
  realizaciones = obtener.realizaciones(data)
  # print(realizaciones)
  return(list(inf = quantile(realizaciones, alfa/2), sup = quantile(realizaciones, 1 - alfa/2)))
}


# Creamos un dataframe para guardar los resultados
cuantiles <- c('Inf Cuantil 25', 'Sup Cuantil 25', 'Inf Cuantil 50', 'Sup Cuantil 50', 'Inf Cuantil 75', 'Sup Cuantil 75')
metodos <-  c('Método 1', 'Método 2')
resultados <- data.frame(matrix(ncol = 6, nrow = 2))
colnames(resultados) <- cuantiles
rownames(resultados) <- metodos

# Calculamos los distintos cuantiles
resultados['Método 1', 'Inf Cuantil 25'] = met.1(data, 0.05, 0.25)$inf
resultados['Método 1', 'Sup Cuantil 25'] = met.1(data, 0.05, 0.25)$sup
resultados['Método 2', 'Inf Cuantil 25'] = met.1(data, 0.05, 0.25)$inf
resultados['Método 2', 'Sup Cuantil 25'] = met.1(data, 0.05, 0.25)$sup
resultados['Método 1', 'Inf Cuantil 50'] = met.1(data, 0.05, 0.5)$inf
resultados['Método 1', 'Sup Cuantil 50'] = met.1(data, 0.05, 0.5)$sup
resultados['Método 2', 'Inf Cuantil 50'] = met.1(data, 0.05, 0.5)$inf
resultados['Método 2', 'Sup Cuantil 50'] = met.1(data, 0.05, 0.5)$sup
resultados['Método 1', 'Inf Cuantil 75'] = met.1(data, 0.05, 0.75)$inf
resultados['Método 1', 'Sup Cuantil 75'] = met.1(data, 0.05, 0.75)$sup
resultados['Método 2', 'Inf Cuantil 75'] = met.1(data, 0.05, 0.75)$inf
resultados['Método 2', 'Sup Cuantil 75'] = met.1(data, 0.05, 0.75)$sup
View(resultados)



m1.cuantil25 = (7.827136, 7.998462)
m1.cuantil50 = (13.02889, 15.9486)
m1.cuantil75 = (28.10737, 33.95662)

m2.cuantil25 = (7.832812, 7.989900)
m2.cuantil50 = (13.07383, 15.8450)
m2.cuantil75 = (27.98185, 33.99426)



