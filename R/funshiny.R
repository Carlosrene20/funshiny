
library(ggplot2)

# Función para crear un diagrama de pastel
crear_pastel <- function(data, variable) {
  ggplot(data, aes_string(x = "", fill = variable)) +
    geom_bar(width = 1) +
    coord_polar("y") +
    theme_void()
}

# Función para crear un diagrama de barras
crear_barras <- function(data, variable) {
  ggplot(data, aes_string(x = variable)) +
    geom_bar(fill = "blue", color = "black") +
    theme_minimal()
}

# Función para crear un histograma
crear_histograma <- function(data, variable) {
  ggplot(data, aes_string(x = variable)) +
    geom_histogram(fill = "blue", color = "black", bins = 30) +
    theme_minimal()
}
# Función para calcular la media
media <- function(data, variable) {
  mean(data[[variable]], na.rm = TRUE)
}

# Función para calcular la mediana
mediana <- function(data, variable) {
  median(data[[variable]], na.rm = TRUE)
}

# Función para calcular la moda
moda <- function(data, variable) {
  ux <- unique(data[[variable]])
  ux[which.max(tabulate(match(data[[variable]], ux)))]
}
