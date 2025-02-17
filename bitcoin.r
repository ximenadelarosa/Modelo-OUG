# Parámetros
n <- 100  # Número de pasos por caminata
num_caminatas <- 5  # Número de caminatas
set.seed(123)  # Para reproducibilidad

# Crear una matriz para almacenar las caminatas
caminatas <- matrix(0, nrow = n, ncol = num_caminatas)

# Generar las caminatas
for (i in 1:num_caminatas) {
  pasos <- sample(c(-1, 1), n, replace = TRUE)
  caminatas[, i] <- cumsum(pasos)
}

# Asignar colores
colores <- rainbow(num_caminatas)

# Gráfica de las caminatas
plot(caminatas[,1], type = "l", col = colores[1], lwd = 2, xlab = "Paso", ylab = "Posición", 
     main = "Múltiples Caminatas Aleatorias 1D", ylim = range(caminatas))

# Añadir las otras caminatas
for (i in 2:num_caminatas) {
  lines(caminatas[, i], col = colores[i], lwd = 2)
}
abline(h = 0, col = "black", lty = 2)
# Añadir leyenda
legend("topright", legend = paste("Caminata", 1:num_caminatas), col = colores, lwd = 2)
