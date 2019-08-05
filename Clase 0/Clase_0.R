# Jesús Alberto Cuéllar Loera
#05/08/2019
# Clase 0


# Pasos básicos -----------------------------------------------------------
2+2
a <- 2
a*a

# importar datos ----------------------------------------------------------


diametro <- c(12,8.6,9.2,7.7,12.9,11.7,9.7,14.2,
              11.8,14.3,12.5)

# Tendencia central -------------------------------------------------------


mean(diametro)
median(diametro)
fivenum(diametro)

# Dispersión --------------------------------------------------------------


sd(diametro)
var(diametro)

# boxplot -----------------------------------------------------------------

boxplot(diametro, horizontal = TRUE, col="lightblue", main="diámetro", 
        xlab="D (cm)")
hist(diametro)


# Importar datos de excel -------------------------------------------------

DB_alturas <- read.csv("C:/MCF202-2019/Datos/alturas.csv", header = T)
head(DB_alturas)

boxplot(DB_alturas$crecimiento)
boxplot(DB_alturas$crecimiento ~ DB_alturas$tratamiento, col="green",
        xlab="Tratamiento", ylab="Crecimiento (cm)",
        main="Efectos del fertilizante" )

