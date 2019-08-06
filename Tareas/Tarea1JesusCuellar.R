# Jesús Alberto Cuéllar Loera 
#06/08/2019 
# Tarea 1


#Cargar datos -------------------------------------------------------------


conjunto <- read.csv("C:/MCF202-2019/Datos/inventario.csv", header = T)
head(conjunto)


# Subset de altura -----------------------------------------------------------

H.media <- subset(conjunto, Altura <= mean(conjunto$Altura))

H16 <- subset(conjunto, Altura < 16.5)


# Subset de vecinos ----------------------------------------------------------

Vecinos3 <- subset(conjunto, Vecinos <= 3)
Vecinos4 <- subset(conjunto, Vecinos > 4)


# Subset de diametro ---------------------------------------------------------

DBHmedia <- subset(conjunto, Diametro < mean(conjunto$Diametro))
DBH16  <- subset(conjunto, Diametro > 16)


# Subset de especie----------------------------------------------------------


EspCedro <- conjunto[(conjunto$Especie == "C"),]
EspRestante <- conjunto[!(conjunto$Especie == "C"),]

DiamCedro16 <- subset(EspCedro, Diametro <= 16.9)
AltCedro18 <- subset(EspCedro, Altura > 18.5)

DiamRestante16 <- subset(EspRestante, Diametro <= 16.9)
AltRestante18 <- subset(EspRestante, Altura > 18.5)



# Histogramas Altura -------------------------------------------------------------

hist(conjunto$Altura, col="green", main="Altura")
hist(H.media$Altura, col="lightblue", main="Alturas <= Media")
hist(H16$Altura, col="aquamarine", main="Alturas < 16.5")


# Histogramas vecinos -----------------------------------------------------


hist(conjunto$Vecinos, col="green", main="Altura")
hist(Vecinos3$Vecinos, col="lightblue", main="Alturas <= Media")
hist(Vecinos4$Vecinos, col="aquamarine", main="Alturas < 16.5")

# histogramas diametro ----------------------------------------------------

hist(conjunto$Diametro, col="green", main="Altura")
hist(DBHmedia$Diametro, col="lightblue", main="Alturas <= Media")
hist(DBH16$Diametro, col="aquamarine", main="Alturas < 16.5")

# estadisticas basicas Altura ---------------------------------------------

mean(conjunto$Altura)
sd(conjunto$Altura)

mean(H.media$Altura)
sd(H.media$Altura)

mean(H16$Altura)
sd(H16$Altura)

# estadisticas basicas vecinos --------------------------------------------

mean(conjunto$Vecinos)
sd(conjunto$Vecinos)

mean(Vecinos3$Vecinos)
sd(Vecinos3$Vecinos)

mean(Vecinos4$Vecinos)
sd(Vecinos4$Vecinos)

# estadisticas basicas diametro -------------------------------------------

mean(conjunto$Diametro)
sd(conjunto$Diametro)

mean(DBHmedia$Diametro)
sd(DBHmedia$Diametro)

mean(DBH16$Diametro)
sd(DBH16$Diametro)

