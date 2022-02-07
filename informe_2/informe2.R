# Calculando valores empíricos para comparar
muestra_promedios <- c()
n1 <- 150
min <- 2
max <- 10

for (i in 1:n1) {
  muestra1 <- runif(n1, min, max)
  muestra_promedios <- c(muestra_promedios, mean(muestra1))
}

mean(muestra_promedios)
sd(muestra_promedios)
var(muestra_promedios)

# Parte a

# Calculo promedio
mu1 <- (min+max)/2
var1 <- (sqrt(((max-min)^2)/12)/sqrt(150))^2

# Parte b
    
# Construir función para intervalos de confianza
unif.inter <- function(datos, alpha = 0.05){
  n = length(datos)
  z = qnorm(1-alpha/2)
  xn = mean(datos)
  k = sd(datos)/sqrt(n)
  intervalo = c(xn-k*z,xn+k*z)
  return (intervalo)
}

unif.inter(runif(n1, min, max))

# Parte c

v <- c()

for (j in 1:100000) {
  v <- c(v, mean(runif(n1, min, max)))
}

hist(v, freq = FALSE, xlab = expression(Media ~ de ~ la ~ variable ~ bar(X[n])), main= expression(Histograma ~ de ~ bar(X[150])), ylab = "Densidad")
curve(dnorm(x,mean=mu1, sd = sqrt(var1)), add = TRUE, lwd=2, col = "#0d2342")

# Parte d

media_en_intervalo <- c()

for (i in 1:n1) {
  muestra <- runif(n1, min, max)
  intervalo <- unif.inter(muestra)
  if  ((intervalo[1] <= mu1)&(intervalo[2]>=mu1)) {
    media_en_intervalo <- c(media_en_intervalo, 0)
  }
  else {
    media_en_intervalo <- c(media_en_intervalo, 1)
  }
}

a <- sum(media_en_intervalo)/150

# Ejercicio 2
n1=150
min=2
max=10
muestra2 <- runif(n1, min, max)

x2=mean(muestra2)
for()
  
  # Ejercicio 3
  
  summary(Grupo.74.datos)
mean(Grupo.74.datos$Muestra_A)
mean(Grupo.74.datos$Muestra_B)
sd(Grupo.74.datos$Muestra_A)
sd(Grupo.74.datos$Muestra_B)

boxplot(Grupo.74.datos,ylim=c(-6,6))
muestraA=Grupo.74.datos$Muestra_A
muestraB=Grupo.74.datos$Muestra_B
cor.test(muestraA,sort(muestraA),method = "spearman")
cor.test(muestraB,sort(muestraB),method = "spearman")

#no hay evidencia para rechazar que Ho sea iid

cor.test(muestraA,muestraB,method = "spearman")

ks.test(muestraA,muestraB)

#hay evidencia suficiente para rechazar Ho (no provienen de la misma dist)

# Ejercicio 4

plot(datos4$income,datos4$happiness,col="hotpink",xlab= "Ingresos", ylab= "Felicidad", main= "Felicidad vs. Ingresos")
lm(datos4$happiness~datos4$income)
help(plot)
summary(lm(datos4$happiness~datos4$income))
abline(lm(datos4$happiness~datos4$income), col="skyblue", lwd=2)







