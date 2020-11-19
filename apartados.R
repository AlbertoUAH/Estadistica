# Script apartados Estadistica
# Author: Alberto Fernandez Hernandez
datos.estatura <- data.frame(alumnos = c("Alumno1", "Alumno2", "Alumno3", "Alumno4", "Alumno5",
                                         "Alumno6", "Alumno7", "Alumno8", "Alumno9", "Alumno10",
                                         "Alumno11", "Alumno12", "Alumno13", "Alumno14", "Alumno15", 
                                         "Alumno16", "Alumno17", "Alumno18", "Alumno19", "Alumno20",
                                         "Alumno21", "Alumno22", "Alumno23", "Alumno24", "Alumno25", 
                                         "Alumno26", "Alumno27", "Alumno28", "Alumno29", "Alumno30"),
                             estatura = c(1.25, 1.28, 1.27, 1.21, 1.22, 1.29, 1.30, 1.24, 1.27, 1.29,
                                          1.23, 1.26, 1.30, 1.21, 1.28, 1.30, 1.22, 1.25, 1.20, 1.28,
                                          1.21, 1.29, 1.26, 1.22, 1.28, 1.27, 1.26, 1.23, 1.22, 1.21))

# apartado a)
# Medidas de posicion central
# Media artimetica
media.aritmetica <- mean(datos.estatura[, "estatura"])
media.aritmetica

# Media geometrica
# Podemos ver que la Media geometrica es ligeramente inferior a la aritmetica
media.geometrica <- Reduce(prod, datos.estatura["estatura"], init = 1) ** (1/nrow(datos.estatura))
media.geometrica

# Mediana
mediana <- median(datos.estatura[, "estatura"])
mediana

# Moda
frecuencias.estaturas <- as.data.frame(table(Estatura = datos.estatura[, "estatura"]))
frecuencias.estaturas[, "Estatura"] <- as.numeric(levels(frecuencias.estaturas[, "Estatura"]))
frecuencias.estaturas <- frecuencias.estaturas[order(-frecuencias.estaturas[, "Freq"]),]
moda <- as.double(frecuencias.estaturas[which(frecuencias.estaturas[, "Freq"] == max(frecuencias.estaturas[, "Freq"])), "Estatura"])
moda

# Medidas de dispersion

# Rango
# Range devuelve los valores maximo y minimo, NO el rango
range(datos.estatura[, "estatura"])
rango <- diff(range(datos.estatura[, "estatura"]))
rango

# Cuasi-varianza
var(datos.estatura[, "estatura"])

# Varianza
varianza <- var(datos.estatura[, "estatura"]) * ((nrow(datos.estatura) - 1 )/ nrow(datos.estatura))
varianza

# Desviacion tipica obtenida a partir de la cuasi-varianza
sd(datos.estatura[, "estatura"])

# Desviacion tipica obtenida a partir de la varianza
desv.tipica <- sd(datos.estatura[, "estatura"]) * sqrt((nrow(datos.estatura) - 1 )/ nrow(datos.estatura))
desv.tipica

# Comparacion del grafico de densidad con una distribucion normal
# con la media y desviacion estandar obtenidas anteriormente
plot(density(datos.estatura[, "estatura"]), type = 'l', ylim = c(0,15),
     lwd = 2, xlab = "Estatura (metros)", ylab = "Densidad",
     main = "Densidad estaturas - distribucion normal")
curve(dnorm(x, mean = media.aritmetica, sd = sd(datos.estatura[, "estatura"])), 
      col = 'red', lwd = 2, type = 'l', add = TRUE)
legend("topleft", legend = c("Densidad estaturas", "Distribucion normal"), 
       col = c("black", "red"), lty = 1, lwd = 2)

# Diagrama de caja y bigotes
boxplot(datos.estatura[, "estatura"], medcol = "red", 
        col = "grey", las = 1, ylab = "ESTATURAS", horizontal = TRUE)

# Mediante la funcion summary mostramos los valores de los cuartiles
summary(datos.estatura[, "estatura"])

# Coeficiente de variacion de Pearson
coef.var.pearson <- desv.tipica / media.aritmetica
coef.var.pearson


# apartado b)
datos.estatura.peso <- data.frame(alumnos = c("Alumno1", "Alumno2", "Alumno3", "Alumno4", "Alumno5",
                                         "Alumno6", "Alumno7", "Alumno8", "Alumno9", "Alumno10",
                                         "Alumno11", "Alumno12", "Alumno13", "Alumno14", "Alumno15", 
                                         "Alumno16", "Alumno17", "Alumno18", "Alumno19", "Alumno20",
                                         "Alumno21", "Alumno22", "Alumno23", "Alumno24", "Alumno25", 
                                         "Alumno26", "Alumno27", "Alumno28", "Alumno29", "Alumno30"),
                             estatura = c(1.25, 1.28, 1.27, 1.21, 1.22, 1.29, 1.30, 1.24, 1.27,
                                          1.29, 1.25, 1.28, 1.27, 1.21, 1.22, 1.29, 1.30, 1.24,
                                          1.27, 1.29, 1.25, 1.28, 1.27, 1.21, 1.22, 1.29, 1.30,
                                          1.24, 1.27, 1.29),
                             peso = c(32, 33, 31, 34, 32, 31, 34, 32, 32, 35, 31, 35, 34, 33, 33, 31,
                                      35, 32, 31, 33, 33, 32, 34, 34, 35, 31, 34, 33, 35, 34))


# Tabla de correspondencias
# Mediante la funcion cut() dividimos el rango de cada columna en un vector de longitud 4
breaks <- as.vector(quantile(datos.estatura.peso[, "estatura"]))
intervalo.estatura <- cut(datos.estatura.peso[, "estatura"], breaks = breaks, include.lowest = TRUE, right = FALSE)
intervalo.peso <- cut(datos.estatura.peso[, "peso"], breaks = 4, include.lowest = TRUE, right = FALSE)

df.intervalos <- data.frame(estatura = intervalo.estatura, 
                            peso = intervalo.peso)
tabla.correspondencias <- table(df.intervalos[, "estatura"], df.intervalos[, "peso"])
tabla.correspondencias

# Calculamos, de forma adicional, las frecuencias marginales tanto de estatura como peso
tabla.correspondencias <- rbind(tabla.correspondencias, apply(tabla.correspondencias, 2, sum))
tabla.correspondencias <- cbind(tabla.correspondencias, apply(tabla.correspondencias, 1, sum))
tabla.correspondencias

# Ejercicio 2
datos.estatura.primeros.15 <- datos.estatura[1:15,]
datos.estatura.ultimos.15 <- datos.estatura[16:30,]

library(car)
# Lo primero de este ejercicio sera comprobar si ambas muestras siguen una distribucion normal, es decir,
# realizar la prueba de normalidad. Una primera aproximacion es de forma grafica, es decir, por medio de
# un grafico q-q y graficos de densidad
mostrar_graficos <- function(datos, columna) {
  par(mfrow = c(1,2))
  qqPlot(datos[, columna], pch=19, las=1, main='QQplot',
        xlab='Cuantiles teoricos', ylab='Cuantiles muestrales')
  plot(density(datos[, columna]), lwd = 3, col = 'blue',
       xlab = 'Altura (metros)', ylab = 'Densidad',
       main = 'Grafico de densidad')
}

# Primeros 15 datos
mostrar_graficos(datos.estatura.primeros.15, "estatura")

# Ultimos 15 datos
mostrar_graficos(datos.estatura.ultimos.15, "estatura")

# Por otro lado, dado que el tamano de cada muestra no supera los 50 elementos, utilizaremos de forma adicional
# el test Shapiro-Wilk en lugar de Kolmogorov-Smirnov
# Primeros 15 datos
shapiro.test(datos.estatura.primeros.15[, "estatura"])

# Ultimos 15 datos
shapiro.test(datos.estatura.ultimos.15[, "estatura"])

# Para contrastar la hipotesis de que ambas varianzas son iguales, podemos hacer una funcion propia...
contrastar.varianzas <- function(x, y, columna, confianza) {
  n.1 <- nrow(x)
  n.2 <- nrow(y)
  var.1 <- var(x[, columna]) * ((n.1 - 1 )/ n.1)
  var.2 <- var(y[, columna]) * ((n.2 - 1 )/ n.2)
  cociente.var <- min(var.1, var.2) / max(var.1, var.2)
  lim.inf <- cociente.var * (1 / qf(1- (1 - confianza) / 2, df1 = n.1 - 1, df2 = n.2 - 1))
  lim.sup <- cociente.var * (1 / qf((1 - confianza) / 2, df1 = n.1 - 1, df2 = n.2 - 1))
  c(lim.inf, lim.sup)
}
contrastar.varianzas(datos.estatura.primeros.15, datos.estatura.ultimos.15, "estatura", 0.95)

# ... o bien utilizar la funcion var.test de R
var.test(datos.estatura.primeros.15[, "estatura"], datos.estatura.ultimos.15[, "estatura"])

# Si establecemos el intervalo de confianza a un 70 %...
var.test(datos.estatura.primeros.15[, "estatura"], datos.estatura.ultimos.15[, "estatura"], conf.level = 0.70)

# Si asumo que ambas varianzas son iguales...
intervalo_confianza <- function(x, y, columna, confianza) {
  n.1 <- nrow(x)
  n.2 <- nrow(y)
  var.1 <- var(x[, columna]) * ((n.1 - 1 )/ n.1)
  var.2 <- var(y[, columna]) * ((n.2 - 1 )/ n.2)
  media.1 <- mean(x[, columna])
  media.2 <- mean(y[, columna])
  aux <- abs(qt(c((1 - confianza) / 2), df = n.1 + n.2 - 2)) * 
          sqrt((n.1 * var.1 + n.2 * var.2) *
          (1/n.1 + 1/n.2))/sqrt(n.1 + n.2 - 2)
  c(media.1 - media.2 + aux, media.1 - media.2 - aux)
}
intervalo_confianza(datos.estatura.primeros.15, datos.estatura.ultimos.15, "estatura", 0.95)

# Una posible alternativa es emplear la funcion t.test...
# Por defecto, conf.level esta a 0.95
t.test(datos.estatura.primeros.15[, "estatura"], datos.estatura.ultimos.15[, "estatura"], var.equal = TRUE, conf.level = 0.95)

# Podemos incluso observarlo de forma grafica a traves de un diagrama de caja y bigotes
par(xpd = TRUE, mar = par()$mar + c(0,7,0,0))
boxplot(datos.estatura.primeros.15[, "estatura"], datos.estatura.ultimos.15[, "estatura"], 
        names = c("Alumnos [1:15]", "Alumnos [16:30]"), 
        xlab = "Altura (m)",
        las = 2, horizontal = TRUE)

# Contraste de hipotesis (varianzas desconocidas pero iguales)
contrastar_hipotesis <- function(x, y, columna, confianza) {
  n.1 <- nrow(x)
  n.2 <- nrow(y)
  var.1 <- var(x[, columna]) * ((n.1 - 1 )/ n.1)
  var.2 <- var(y[, columna]) * ((n.2 - 1 )/ n.2)
  media.1 <- mean(x[, columna])
  media.2 <- mean(y[, columna])
  s.c <- (n.1 * var.1 + n.2 * var.2) / (n.1 + n.2 - 2)
  t <- (media.1 - media.2) / sqrt(s.c *  (1/n.1 + 1/n.2))
  t.student <-  abs(qt(c((1 - confianza) / 2), df = n.1 + n.2 - 2))
  data.frame("T" = t, "t.Student" = t.student)
}
contrastar_hipotesis(datos.estatura.primeros.15, datos.estatura.ultimos.15, "estatura", 0.95)
