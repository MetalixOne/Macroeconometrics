setwd("C:/Users/mca08/Downloads/Macroeconometría/Laboratorio 1")
library(AER)
library(ggplot2)
df <- read.csv("Datos Ingresos.csv")
head(df)
plot(1:10)
plot(1:10, t='l')
#par(mfrow = c(2,1)) this works for putting several together
plot(1:10, t='o')
plot(1:10, t='o', main = "Prueba", col = 'purple', ylab = "Eje de las y",
     xlab = 'Eje de las x')
colnames(df)
summary(df[,1]) #En los corchetes es primero fila y luego columna
dim(df)
summary(df[,1])[4]
summary(df[,1])[c(1,4)]
mean(df[,1])
summary(df)[,c(1,2)]
table(df$WAGE) #Da la tabla de frecuencias
var(df$WAGE)
var(df[,c(1,2)])
cov(df[,c(1,2)])
cor(df$WAGE, df$AGE)
reg <- lm(df$WAGE ~ df$AGE) #linear model
summary(reg)
reg2 <- lm(df$WAGE ~ df$AGE + df$EDUCATION)
summary(reg2)
reg3 <- lm(WAGE ~ AGE + EDUCATION, data = df)
summary(reg3)
reg4 <- lm(WAGE ~ AGE + EDUCATION -1, data = df) #-1 eliminates the intercept.
#Another option is writing +0
summary(reg4)
reg2$residuals
plot(reg2$residuals)
summary(reg2$residuals)
reg2$coefficients
coefficients(reg2)
#Las dos de arriba hacen lo mismo
confint(reg2)
confint(reg2, level = 0.99)
test <- df
datostemp <- 1:10218
test2 <- cbind(test, datostemp) #combina test y datostemp
vif(reg2)#Nos da el vif de cada variable
