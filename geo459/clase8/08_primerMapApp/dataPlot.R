dataPlot <- function(datos , campo){

  ggplot(data = datos, aes_string(x = campo)) + geom_histogram() + theme_bw() +
    xlab(campo) + ylab('Frecuencia')
}