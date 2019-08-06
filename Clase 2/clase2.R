
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
boxplot(vivero$IE ~ vivero$Tratamiento, col="grey", main="A")
barplot(tapply(vivero$IE, list(vivero$Tratamiento), mean ), beside=T, main="B")

# Paried t-test -----------------------------------------------------------

test.tiempo <- t.test(vivero$IE ~ vivero$Tratamiento, paired=TRUE)
test.tiempo



