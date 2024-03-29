#Jesús Cuéllar Loera
#09/Agosto/2019
#Clase 5


# ANOVA -------------------------------------------------------------------

arena <- c(6,10,8,6,14,17,9,11,7,11)
arcilla <- c(17,15,3,11,14,12,12,8,10,13)
limo<- c(13,16,9,12,15,16,17,13,18,14)

y.ton <- c(arena, arcilla, limo)
suelo<- gl(3,10,30, labels=c("arena","arcilla","limo"))
prod<- data.frame(suelo, y.ton)
head(prod)

tapply(prod$y.ton, prod$suelo, mean)
tapply(prod$y.ton, prod$suelo, var)

shapiro.test(prod$y.ton)
bartlett.test(prod$y.ton, prod$suelo)
fligner.test(prod$y.ton, prod$suelo)

boxplot(prod$y.ton ~ prod$suelo, xlab = "Tipo de suelo",
        ylab= "Ton/ha", col="green")

aovsuelo<- aov(prod$y.ton ~ prod$suelo)
aovsuelo
summary(aovsuelo)
par(mfrow=c(2,2))
plot(aov(prod$y.ton~prod$suelo))
par(mfrow=c(2,2))

TukeyHSD(aovsuelo, conf.level = 0.95)

plot(TukeyHSD(aovsuelo))
summary.lm(aovsuelo)

#se acepta la hipótesis alternativa debido a que al menos
#una media (en este caso entre la arcilla y la arena) tiene una diferencia significativa 

