lapply(c('AER','car','sandwich','lmtest'), library, character.only=T)

#Cargar la base de datos
data("CigarettesB")
df <- CigarettesB
rm(CigarettesB)

#Correr una regresión y graficar los residuales
reg <- lm(df$packs ~ df$price)
summary(reg)
plot(reg$residuals, ylab = "Residuals", xlab = "")

#Hacer a mano la puerba Goldfeld-Quandt
#Ordenar la base de datos en base a los datos del precio, y crear dos bases de datos nuevas con 
#solo los datos más bajos y altos del precio
dfo <- df[order(df$price, decreasing = F),]
df1 <- dfo[1:15,]
df2 <- dfo[32:46,]

#Hacer la regresión y obtener los residuales de las dos nuevas bases de datos
reg1 <- lm(df1$packs ~ df1$price)
res1 <- matrix(reg1$residuals)
plot(reg1$residuals)
reg2 <- lm(df2$packs ~ df2$price)
res2 <- matrix(reg2$residuals)
plot(reg2$residuals)

#Obtener el estadístico de prueba
(t(res2)%*%res2/13)/(t(res1)%*%res1/13)
qf(p = 0.05, df1 = 13, df2 = 13, lower.tail = F)

#El estadístico de prueba, se encuentra en la zona de no rechazo, por lo tanto no rechazamos la hipótesis nula de que hay homoscedasticidad
gqtest(lm(df$packs ~ df$price), order.by = df$price, fraction = 16)

#Se corre la regresión agregando el ingreso
reg3 <- lm(df$packs ~ df$price + df$income)
summary(reg3)
plot(reg3$residuals, ylab = "Residuales", xlab = "")

#La prueba de Goldfeld-Quandt
gqtest(reg3, order.by = df$price, fraction = 12)
gqtest(reg3, order.by = df$income, fraction = 12)

#Test de Breusch-Pagan
bptest(reg3, studentize = F)

#Test de White
bptest(reg3, ~ df$price^2 + df$income^2 + df$price*df$income)

#Revisar la varianza corregida por White
vcovHC(reg3)
vcov(reg3)