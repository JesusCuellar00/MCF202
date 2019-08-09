#Jesús Alberto Cuéllar
#09/Agosto/2019
#Clase 4

#mide la relacion lineal entre dos variables
#coeficiente de correlacion(r) -1,1

# Lectura de datos --------------------------------------------------------

library(repmis)
erupcion <- source_data("https://www.dropbox.com/s/liir6sil7hkqlxs/erupciones.csv?dl=1")

plot(erupcion$waiting , erupcion$eruptions, pch= 10,
     xlab= "Tiempo de espera (min)",
     ylab= "Duración (min)")

library(pastecs)

stat.desc(erupcion$eruptions,basic= FALSE, norm= TRUE)
shapiro.test(erupcion$eruptions)

shapiro.test(log(erupcion$eruptions))


meanErup <- mean(erupcion$eruptions)
meanWai <- mean(erupcion$waiting)

sdErup <- sd(erupcion$eruptions)
sdWai <- sd(erupcion$waiting)


cor.test(erupcion$eruptions,erupcion$waiting, method = "pearson" )

cor.test(erupcion$eruptions,erupcion$waiting, method= "spearman")



# Regresion lineal --------------------------------------------------------

#H0 = la predicción no es significativa
#Ha = la predicción es significativa

lm.erup <-lm(erupcion$eruptions ~ erupcion$waiting)
plot(erupcion$waiting , erupcion$eruptions, pch= 10,
     xlab= "Tiempo de espera (min)",
     ylab= "Duración (min)")
abline(lm.erup, col="blue")
text(52,4.5, "Y = -1.87 + 0.07*x")
text(52, 4, "r^2 = 0.81")

lm.erup
summary(lm.erup)
length(erupcion$eruptions)
(0.9)^2

y.60<- -1.87+0.07*60
y.60

espera<- erupcion$waiting
duracion<- erupcion$eruptions

res<-resid(lm.erup)
res
sum(res)

pre <- fitted(lm.erup)
res.2 <- res^2
cuadro<- round(data.frame(espera, duracion,pre,
                    res, res.2), 4)

SSE<- sum((duracion - pre)^2)
SSE
vari<- SSE/length((erupcion$eruptions)-2)
vari


# pureba hipotesis regresion ----------------------------------------------
an.erup <- anova(lm.erup)
an.erup

#aceptamos la hipotesis alternativa que nos dice que
# el modelo de regresion es significativo

# Ejercicios ebanos -------------------------------------------------------

ebanos <-read.csv("C:/MCF202-2019/Datos/ebanos.csv", header= T)

plot(ebanos$altura , ebanos$diametro, pch= 10,
     xlab= "Diametro (cm)",
     ylab= "Altura(m)")

library(pastecs)

stat.desc(ebanos$altura,basic= FALSE, norm= TRUE)
shapiro.test(ebanos$altura)
shapiro.test(ebanos$diametro)

shapiro.test(sin(ebanos$altura))

cor.test(ebanos$altura,ebanos$diametro, method = "pearson" )

cor.test(ebanos$altura,ebanos$diametro, method= "spearman")

lm.eba <-lm(ebanos$altura ~ ebanos$diametro)
plot(ebanos$diametro , ebanos$altura, pch= 10,
     xlab= "Diametro)",
     ylab= "Altura")
abline(lm.eba, col="blue")

lm.eba
summary(lm.eba)

(0.82)^2


