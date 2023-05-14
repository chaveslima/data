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
Cest=
R=Renda;C=Consumo
for(i in 1:26){
  segments(R[i],C[i],R[i],Cest[i], col="blue", lty=2)}
#
# Extraindo as estatisticas individualmente
e=resid(reg);e
b0=coefficients(reg)[1];b0
b1=coefficients(reg)[2];b1
Sb0=summary(reg)$coefficients[, 2][1];Sb0
Sb1=summary(reg)$coefficients[, 2][2];Sb1
t0=b0/Sb0;t0
t1=b1/Sb1;t1
####
anova(reg)
###
SQR=anova(reg)[1,2];SQR # Soma do Quadrado da Regressao
SQE=anova(reg)[2,2];SQE # Soma do Quadrado dos Erros
SQT = SQR + SQE;SQT # SQT=Soma dos Quadrados Totais
R2=SQR/SQT;R2 # R-quadrado
#
QMR=anova(reg)[1,3];QMR # Quadrado Medio da Regressão
QME=anova(reg)[2,3];QME # Quadrado Medio dos Erros
F=QMR/QME;F # F de Snedecor
#
#########################
####### Exercicio #######
#########################
install.packages('WDI')
library(WDI)
#
##  Coreia de 1960 a 2021
# GDP current U$
X=WDI(country="KOR", indicator = "NY.GDP.MKTP.CD")
X
x=X[2:47,5]
plot.ts(rev(x))
#
# EXPORTS current U$
Y=WDI(country="KOR", indicator = "BX.GSR.TOTL.CD")
y=Y[2:47,5]
# Transformar em biloes
PIB=y/(10**9)
EXPO=x/(10**9)
# Regressao linear
reg=lm(PIB~EXPO)
summary(reg)
# Grafico
plot(EXPO,PIB, col="blue",pch=19, ylab="PIB (U$ Bi)",
     xlab="EXPO (U$ Bi)")
abline(reg,col="red")
grid()
