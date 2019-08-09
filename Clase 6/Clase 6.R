#Jesús Cuéllar Loera
#09/Agosto/2019
#Clase 6



# Covarianza --------------------------------------------------------------

library(repmis)
edad <- source_data("https://www.dropbox.com/s/nxoijhgmutuho0s/datos_control_Rascon.csv?dl=1")

head(edad)
str(edad)

#Identificar columna SP como factor
edad$SP<- factor(edad$SP)
str(edad)


# separar por factores ----------------------------------------------------

ariz<- subset(edad, SP=="arizonica")
ariz.lm<- lm(ariz$EDAD~ariz$DAP)
summary(ariz.lm)
dura<- subset(edad, SP="durangensis")
dura.lm<- lm(dura$EDAD~dura$DAP)
summary(dura.lm)
# Regresion dos factores --------------------------------------------------

cov.edad<- lm(edad$EDAD~edad$DAP + edad$SP)
summary(cov.edad)

plot(edad$DAP[edad$SP ==	"arizonica"], edad$EDAD[edad$SP ==	"arizonica"], col= "orange", pch="A", xlim = c(0,50), ylim=c(0,130))
abline(cov.edad$coefficients[1],cov.edad$coefficients[2], col="orange")
text(30,20, "Ya(-7.65 + 1.98*x)")
points(edad$DAP[edad$SP=="durangensis"], edad$EDAD[edad$SP=="durangensis"],  col= "green", pch="D", xlim = c(0,50), ylim=c(0,130))
abline(cov.edad$coefficients[1] + cov.edad$coefficients[3], cov.edad$coefficients[2], col="green", lty="dashed")
text(19,100, "Yd(11.41 + 1.98*x)")