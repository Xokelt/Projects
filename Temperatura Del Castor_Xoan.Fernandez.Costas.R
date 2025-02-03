##########################
# Temperatura del Castor #
##########################

# Construir un modelo que permita predecir la temperatura corporal 
# de un castor teniendo en cuenta la hora del día y el hecho de si 
# el castor tiene actividad fuera de su refugio o no. ¿Se cumplen 
# las hipótesis estructurales necesarias para poder formular el modelo?


# Borramos el ambiente de trabajo
rm(list=ls())

# Localizar el directorio de trabajo, cambiarlo y leer el fichero de datos
datos<- beaver2
summary(datos)

# Datos para graficar la temperatura de los castores frente el tiempo
require(graphics)
yl <- range(beaver2$temp)
beaver.plot <- function(bdat, ...) {
  nam <- deparse(substitute(bdat))
  with(bdat, {
    # Hours since start of day:
    hours <- time %/% 100 + 24*(day - day[1]) + (time %% 100)/60
    plot (hours, temp, type = "l", ...,
          main = paste("Temperatura corporal"))
    abline(h = 37.5, col = "gray", lty = 2)
    act <- activ == 1
    points(hours[act], temp[act], col = 2, cex = .8)
  })
}
# Hora formateada
hours <- datos$time %/% 100 + 24*(datos$day - datos$day[1]) + (datos$time %% 100)/60

# Cambio de datos
datos$time <- hours

# Grafico de la temperatura corporal de los castores
beaver.plot(beaver2, ylim = yl, ylab = "Temperatura (ºC)", xlab= "Horas (h)")

 # Utilizamos la función subset, para agrupar los datos en activos y no activos
reposo <- subset(beaver2, beaver2$activ=="0")
activos <- subset(beaver2, beaver2$activ=="1")

summary(activos)
summary(reposo)

 # Otra forma de representación esquemática
library(ggplot2)
beaver2$activ<-ifelse(beaver2$activ==1,"Active","Dormant")
qplot(time,temp, 
      xlab= "Tiempo", ylab = "Temperatura", main = "Gráfico de dispersión Temperatura vs Tiempo",
      colour=activ,
      facets=activ~.,
      geom=c("point","smooth"),data=beaver2)+ 
  theme_bw(base_size = 12, base_family = "")

# Para abreviar ...
time<-datos$time
temp<-datos$temp
day<-datos$day
activ<-datos$activ

 # Resumen para variables cualitativas (aunque estén codificadas como numéricas!!!)
table(datos$activ)

 # Gráficos que podemos hacer para las variables cualitativas
pie(table(datos$activ), col=c(2,3), main = "Gráfico de sectores para la actividad")
# Leyenda

legend("right",legend = c("En reposo","Activos"), fill = c(2,3), title = "Actividad física")

 # Gráficos para variables numéricas. Temp
hist(datos$temp,main = "Histograma para la temperatura", xlab= "Temperatura (ºC)", ylab = "Frecuencia", col=4)
boxplot (datos$temp, main = "Diagrama de cajas para la temperatura", ylab= "Temperatura (ºC)", col=3)


# Algunos analisis exploratorios

# Gráfico de dispersión tiempo vs temperatura (coloreados según los castores estean activos o en reposo)
plot(time,temp,col=2+activ, pch=19, ylab = "Temperatura", xlab = ("Tiempo"), main = "Gráfico de dispersión Temperatura vs Tiempo") # En esta ocasión utilizamos los valores de smoker para representar las 3 variables en un mismo gráfico


# Media de la temperatura de los castores en reposo
mean(temp[activ==0])
 # Media de la temperatura de los castores activos
mean(temp[activ==1])

 # Diagrama de cajas para ver la distribución de los datos según se encuentren en reposo o activos.
boxplot(temp[activ==0],temp[activ==1], col = c(2,3), xlab= "Actividad física", ylab = "Temperatura (ºC)", names = c("En reposo","Activos"), main = "Diagrama de cajas Temperatura vs Actividad Física")

# Observación de la varianza y desviación típica
sd(activos$temp)
sd(reposo$temp)
var(activos$temp)
var(reposo$temp)

# Estimación de la densidad de la temperatura de los castores y contraste de normalidad sobre dicha variable
plot(density(temp), main = "Gráfico de densidad para la temperatura", xlab= "Temperatura (ºC)", ylab = "Frecuencia", col=4)


shapiro.test(temp)

# Importante: El test de saphiro s un test de normalidad. H0, los datos siguen una distribución normal
# H1: Los datos NO siguen una distribución normal
# p-valor = 7.764e-05 < apha (0.05) -> Rechazamos H0, no podemos asumir que la variable temp
# siga una distribuciñon normal

# Correlación de los datos
library(Rcmdr)
cor(datos[,c("time","temp","activ")], use="complete")
scatterplotMatrix(~activ+temp+time, regLine=FALSE, smooth=FALSE, 
                  diagonal=list(method="density"), data=datos)

library(corrplot)
corrplot(cor(datos[, c("time","temp","activ")], method = "pearson"))

library(polycor)
cor_rango_biserial_variable1 <- polychor(activ, temp, fixpar = c(0.001, 0.999))
cor_rango_biserial_variable2 <- polychor(activ, time)


# Ajuste del modelo lineal m?ltiple: 
# Temperatura de los castores en función del tiempo (hora) y de su actividad fuera del refugio
rlm1<-lm(temp ~ time + activ)
rlm1
# y = intercepto + time*x1 + activ*x2. UTILIZAR UN NIVEL DE CONFIANZA DEL 90%.

summary(rlm1)

 # Podemos comprobar como el R cuadrado ajustado nos da un valor del 0,778, por lo cual nos sugiere que este puede ser un buen
 # modelo de predicción para hallar la temperatura de los castores

# Intervalos de confianza para los parámetros
confint.lm(rlm1, level=0.90) # Con esto podemos comprobar la variabilidad inherente de los datos de la muestra.
 # De esta manera proporciona un rango plausible de valores para los verdaderos parámetros

# contraste de la T:
summary(rlm1)$coefficients

# Gr?fico de valores predichos y observados de la respuesta (Temperatura de los castores)
predichos<-rlm1$fitted.values
plot(predichos,temp, ylab= "Temperatura (ºC)", xlab = "Valores predichos", col=4)
abline(0,1,col="red")
 # Como podemos comprobar en base al gráfico de dispersión existe una cierta linealidad.

# Gr?fico de valores predichos frente a residuos
residuos<-rlm1$residuals
plot(predichos,residuos, ylab= "Residuos", xlab = "Valores predichos", col=4)
abline(0,0,col="red")


# Contraste de regresión (significatividad de la variable x). Bondad de ajuste
anova(rlm1)
# Podemos comprobar que el p - valor de las diferentes variables es inferior a alfa = 0.1
# entonces rechazamos la hipotesis nula y aceptamos la H1, donde el efecto lineal de las variables
# explicativas es estadísticamente sígnificativo. Se rechaza que el modelo lineal no es significativo

# Cálculo de los residuos estandarizados
residuos_estandarizados<-rstandard(rlm1)

# Gráficos de residuos: histograma, caja y bigotes, estimador núcleo (kernel)
hist(residuos_estandarizados, freq=FALSE, main="Distribución de los residuos estandarizados", xlab="Residuos Estandarizados", ylab= "Densidad", col = 4)
xnorm<-seq(min(residuos_estandarizados),max(residuos_estandarizados),length=40)
ynorm<-dnorm(xnorm)
lines(xnorm, ynorm,col="blue")
boxplot(residuos_estandarizados, main="Diagrama de cajas de los residuos estandarizados", col = 4)
plot(density(residuos_estandarizados), main="Densidad de los residuos estandarizados", xlab="Residuos Estandarizados", ylab= "Densidad", col = 4)

# Gráficos de residuos por defecto: 
# frente a predicho, Q-Q, escala-localizaci?n, frente al leverage
plot(rlm1, col = 4)

# Normalidad de los residuos: QQ plot y contrastes de Shapiro-Wilks,
# Kolmogoroff-Smirnoff-Lilliefors, Cram?r - von Mises y chi-cuadrado
# install.packages("EnvStats")
library(car)
qqPlot(rlm1,main="QQ plot")
shapiro.test(residuos_estandarizados)
# Importante: El test de saphiro s un test de normalidad. H0, los datos siguen una distribución normal
# H1: Los datos NO siguen una distribución normal
# p-valor = 0,83 > apha (0.1) -> Aceptamos en esta ocasion la hipótesis nula, asumiendo que los residuos estandarizados siguen
# una distribución normal


# install.packages("nortest")
library(nortest)
lillie.test(residuos_estandarizados)
# El lillie test, es una prueba de bondad de ajuste para evaluar si una muestra
# sigue una distribución teoríca específica.
# Hipotesis H0: La muestra sigue una dsitribución teórica
# Hipotesis H1: La muestra no siguen una distribución teórica
# En este caso como podemos comprobar p-value = 0.683 por lo que los datos siguen
# Una distribución teórica específica. En este caso sería una distribución normal.

cvm.test(residuos_estandarizados)
# Se utiliza para evaluar la bondad de ajusta de una muestra a una distribución
# teórica continua, este caso es una distrinución normal
# Hiporesis H0. La muestra sigue una distribución normal
# Observando los resultado llegamos a la conclusión que p-value = 0.468 > alfa (0.1)
# entonces no se rechaza la hipotesis nula y la muestra sigue una distribución normal

pearson.test(residuos_estandarizados)
# Test de normalidad. H0: se asume una distribución normal. Como vemos p-value = 0.116, se asume que los datos
# siguen una distribución normal

# Contraste de homocedasticidad de Breusch-Pagan. También es un constraste de linealidad (ver mejor)
# install.packages("lmtest")

library(lmtest)
bptest(rlm1,studentize = F)
# Permite evaluar la homocedasticidad en un modelo de regresión
# Donde la H0: La varianza de los errores del modelo es cosntante a lo largo de todos los niveles
# de las variables explicativas. Como comprobamos p-value = 0.1182, 
# no se rechaza la hipotesis nula.


# Outliers (datos atípicos): contraste de residuos atípicos, QQ plot de residuos
# install.packages("car")
library(car)
outlierTest(rlm1)
# El rest de Bonferroni, nos indica que utilizando un distribución t, podemos comprobar
# el estado atípico de lso resudios estudentizados. Por lo que un valor significativo indica
# un valor atípico extremo que justifica un extudio del mismo.
# H0: No hay presencia de un valor atípico
# Como observamos el p-valor = 0.391, por lo que no hay presencia de un valor atípico

# outlierTest(rlm1,cutoff = Inf,n.max=Inf)
qqPlot(rlm1,main="QQ plot", xlab="Cuantil t", ylab="Residuos Estudentizados") # Buscar porque no da entorno a 0

# Observaciones influyentes
# Gráfico de variables añadidas
avPlots(rlm1, main="Gráfico de variables añadidas", ylab = "Temperatura | Otros")

# Contrastes de aleatoriedad: Durbin-Watson, Ljung-Box y rachas
durbinWatsonTest(rlm1)
Box.test(residuos_estandarizados,type = "Ljung-Box",lag=5)
# Permite evaluar si existe autocorrelación de primer orden 
# en los residuos de un modelo de regresión
# Hipotesis H0: No esiste autocorrelación de primer orden en los residuos del modelo
# Hipotesis H1: Existe autocorrelación de primer orden en los residuos del modelo
# Teniendo en cuneta que el p-value = 0, < alfa (0.1), entonces rechazamos la H1 y aceptamos
# la H1, indicando que existe autocorrelación en los residuos, es decir lo residuos consecutivos 
# no son independientes. Del mismo modo ocurre para el test de Box-Ljung con un p valor p-value < 2.2e-16.


# install.packages("randtests")
library(randtests)
runs.test(residuos_estandarizados)
# Es una prueba estadística no paramétrica que se utiliza para evaluar
# la aleatoriedad en una secuencia de datos. Donde:
# La hipotesis H0: Los datos siguen un patrón aleatorio
# La hipotesis H1: Los datos no siguen un patrón aleatorio
# Como podemos comprobar tenemos un p-valor = 4.618e-12 < alfa (0.05),
# Entonces rechazamos H0 y  aceptamos H1, por lo que los datos no siguen
# un patron aleatorio


# Homocedasticidad: contraste de varianza no constante y residuos frente a valor predicho
ncvTest(rlm1)
# Para comprobar esto este test es un test de homocedasticidad para comprobar
# si la varianza de los residuos de un modelo de regresión
# es constante. Para ello:
# Hipotesis H0: La varianza de los errores es constante (homocedasticidad).
# Hipotesis H1: La varianza de los errores no es constante (heterocedasticidad).

help(spreadLevelPlot)
spreadLevelPlot(rlm1)
# En el gráfico claramente se muestra un patron de agrupación continuo
# dejando claro que los datos tiene homocedasticidad

# Multicolinealidad: factor de inflación de la varianza (VIF) y variables con VIF>2
vif(rlm1) # variance inflation factors
sqrt(vif(rlm1)) > 2
# Como vemos gracias a FIV podemos detectar una varianza y podemos comprobar 
# el VIF no es mayor que 2, y mucho menos mayor que 10 por lo que, 
# podemos asegurar que no sufrimos un problema de multicolinealidad
# en nuestros datos

# Mis diagnosis: contraste global de suposiciones del modelo
# install.packages("gvlma")
library(gvlma)
globalmodelo <- gvlma(rlm1)
summary(globalmodelo) 



# Algunos graficos mas de residuos: frente a la respuesta, a la temperatura y actividad del castor
plot(temp,residuos_estandarizados)
plot(time,residuos_estandarizados)


# PAra la realización de predicciones Predicciones
beaver2
Hora_del_Dia <- 1200 # Escribir la hora del como hora/minuto. De esta manera las 12:30 se escribe: 1230
Hora_del_Dia_Formateada <- Hora_del_Dia %/% 100 + (Hora_del_Dia %% 100)/60
Actividad_Fisica <- 0 # 1 para la actividad física y 0 para no actividad física

valor_real <- subset(beaver2, beaver2$time == Hora_del_Dia)
prediccion <- predict(rlm1, newdata = data.frame(time = Hora_del_Dia_Formateada, activ = Actividad_Fisica))

 # Comparación
prediccion
valor_real$temp


