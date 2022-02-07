# Ejercicio 3
## Parte a
datos <- runif(1000, c(-2,1), c(-1,2))
hist(datos)

## Parte b
dist <- rnorm(1000,3*datos-1, .5)

## Parte c
hist(dist, freq=FALSE)
LcKS(dist, "pnorm")

## Parte d
modelo_lineal <- summary(lm(datos~dist))
plot(datos, dist)
abline(lm(datos~dist))

## Parte e Test de normalidad
hist(modelo_lineal$residuals)
residuos <- modelo_lineal$residuals
LcKS(residuos, "pnorm")
b <- dist[which(dist<0)]

