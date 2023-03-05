lapply(c('AER','orcutt','prais','lmtest'), library, character.only=T)

data("USConsump1993")
df <- as.data.frame(USConsump1993)
rm(USConsump1993)

reg <- lm(expenditure ~ income, data = df)
summary(reg)
plot(reg$residuals, ylab = "Residuales", xlab = "")
