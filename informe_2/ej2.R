3n1 <- 150
min <- 2
max <- 10
datos <- runif(n1, min, max)

calcula_pvalor <- function(datos, min, max) {
  mu0 <- (min+max)/2
  mean <- mean(datos)
  sd <- sd(datos)
  n <- length(datos)
  p_valor <- 2*(1-(pnorm(sqrt(n)/sd * abs(mean-mu0))))
  return(p_valor)
}

calcula_pvalor(datos)

promedio_muestras <- c()
pvalores_muestras <- c()
for (i in 1:1000) {
  muestrai <- runif(n1, min, max)
  promedio_muestras <- c(promedio_muestras, mean(muestrai))
  pvalores_muestras <- c( pvalores_muestras, calcula_pvalor(promedio_muestras, min, max))
}
