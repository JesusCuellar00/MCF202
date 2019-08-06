
# Importar datos vivero ---------------------------------------------------


vivero <-read.csv("C:/MCF202-2019/Datos/plantulas.csv", header = T)
head(vivero)

summary(vivero)

# T una muestra -----------------------------------------------------------

boxplot(vivero$IE)
t.test(vivero$IE, mu= 0.85)
#La media observada no muestra una diferencia significativa debido a
# que el valor de p es mayor que el alfa establecido (0.05) ademas que 
# la media teoretica se encuentra dentro del rango de los valores de intervalo
t.test(vivero$IE, mu= 0.9)
# La media muestra una diferencia a la media teorética, por lo cual se acepta la hipotesis alterna ya que el valor
# de p (0.01) es menor que el valor del alfa establecido (0.05)

boxplot(vivero$IE ~ vivero$Tratamiento, col="orange", 
        xlab= "Tratamiento", ylab="IE")


# Prueba muestras independientes --------------------------------------------------

var.test(vivero$IE ~ vivero$Tratamiento)

t.test(vivero$IE ~ vivero$Tratamiento, var.equal=T)
#Existe una diferencia significativa entre el indice de esbeltes de las plantulas con fertilizante y las que no
#El valor de p (0.004) comprueba que el fertilizante mejora el IE


op <- par(mfrow=c(1,2), cex.axis=.7,  cex.lab=.9)
boxplot(vivero$IE ~ vivero$Tratamiento, col="green", main="A")
barplot(tapply(vivero$IE, list(vivero$Tratamiento), mean ), beside=T, main="B")

# Paried t-test -----------------------------------------------------------

test.tiempo <- t.test(vivero$IE ~ vivero$Tratamiento, paired=TRUE)
test.tiempo


# Ejercicio producción ----------------------------------------------------
inventario <-read.csv("C:/MCF202-2019/Datos/produccion.csv", header = T)
head(inventario)

boxplot(inventario$Kgsem ~ inventario$Tiempo, col="green")
t.test(inventario$Kgsem ~ inventario$Tiempo, paired = T)

boxplot(inventario$Germ ~ inventario$Tiempo, col="green")
t.test(inventario$Germ ~ inventario$Tiempo, paired = T)

#T2012 <- subset(inventario, Tiempo == "T2012")
#mean2012=mean(T2012$Germ)

#T2013 <- subset(inventario, Tiempo == "T2013")
#mean2013=mean(T2013$Germ)

tapply(inventario$Germ, inventario$Tiempo, mean)
