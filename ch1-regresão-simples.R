set.seed(123)
Renda=seq(2,12,by=0.4)
e=rnorm(Renda,0,2)
Consumo = 1.2 + 0.98*Renda + 0.6*e
Renda
round(Consumo,1)
#
plot(Renda,Consumo, col="blue",pch=19, ylab="Consumo (R$ 1000,00)",
            xlab="Renda (R$ 1000,00)",ylim=c(2,14))
reg = lm(Consumo~Renda)
summary(reg)
abline(reg,col="red")
grid()
#
Cest=fitted(reg)
#
for(i in 1:26){
  segments(R[i],C[i],R[i],Cest[i], col="blue", lty=2)}
#

