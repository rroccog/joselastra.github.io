#Función básica para ploteo GEO 459 clase 8 07.10.2020
dataPlot <- function(datos , campo){
  ggplot(data = datos, aes_string(x = campo)) + geom_histogram() + theme_bw() +
    xlab(campo) + ylab('Frecuencia')
}