
indice <-read.csv("C:/MCF202-2019/Datos/plantulas.csv", header = T)
head(indice)

boxplot(indice$IE ~ indice$Tratamiento, col="grey")

summary(indice)

# T una muestra -----------------------------------------------------------


t.test(indice$IE, mu= 0.85)
boxplot(indice$IE)

# Control -----------------------------------------------------------------


Ctrl <- subset(indice, Tratamiento == "Ctrl")

mean=mean(Ctrl$IE)
fivenum(Ctrl$IE)
sd=sd(Ctrl$IE)


shapiro.test(Ctrl$IE)

ks.test(Ctrl$IE, "pnorm", mean, sd)
boxplot(Ctrl$IE,horizontal = TRUE, col= "green", main="IE")
hist(Ctrl$IE, col= "green", main="IE")

# Fertilizante ------------------------------------------------------------


Fert <- subset(indice, Tratamiento == "Fert")
meanF=mean(Fert$IE)
fivenum(Fert$IE)
sdF=sd(Fert$IE)

shapiro.test(Fert$IE)

ks.test(Fert$IE, "pnorm", meanF, sdF)

boxplot(Fert$IE,horizontal = TRUE, col= "green", main="IE")
hist(Fert$IE, col= "green", main="IE")



# Normalidad de varianza --------------------------------------------------

var.test(Ctrl$IE, Fert$IE)


# #Prueba T de student ----------------------------------------------------

t.test(Ctrl$IE, Fert$IE, var.equal=T)

t.test(Ctrl$IE, Fert$IE)

op <- par(mfrow=c(1,2), cex.axis=.7,  cex.lab=.9)
boxplot(indice$IE ~ indice$Tratamiento, col="grey", main="A")
barplot(tapply(indice$IE, list(indice$Tratamiento), mean ), beside=T, main="B")


# T dos muestras ----------------------------------------------------------

semillas <-read.csv("C:/MCF202-2019/Datos/semillas.csv", header = T)
head(semillas)

# T2010 -------------------------------------------------------------------


T2010 <- subset(semillas, tiempo == "T2010")

mean2010=mean(T2010$Kilogramos)
fivenum(T2010$Kilogramos)
sd2010=sd(T2010$Kilogramos)

shapiro.test(T2010$Kilogramos)

ks.test(T2010$Kilogramos, "pnorm", mean2010, sd2010)
boxplot(T2010$Kilogramos,horizontal = TRUE, col= "green", main="Kg")
hist(T2010$Kilogramos, col= "green", main="Kg")


# 2013 --------------------------------------------------------------------


T2013 <- subset(semillas, tiempo == "T2013")

mean2013=mean(T2013$Kilogramos)
fivenum(T2013$Kilogramos)
sd2013=sd(T2013$Kilogramos)

shapiro.test(T2013$Kilogramos)

ks.test(T2013$Kilogramos, "pnorm", mean2013, sd2013)
boxplot(T2013$Kilogramos,horizontal = TRUE, col= "green", main="Kg")
hist(T2013$Kilogramos, col= "green", main="Kg")


# Paried t-test -----------------------------------------------------------

test.tiempo <- t.test(semillas$Kilogramos ~ semillas$tiempo, paired=TRUE)
test.tiempo


