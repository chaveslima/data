set.seed(123)
# gerar 2 erros aleatorios
e1=rnorm(26,0,1)
e2=rnorm(26,0,1)
# Gerar variavel Renda
Renda=seq(2,12,by=0.4) + e1
# Gerar variável consumo
Consumo = 4 + 0.98*Renda + e2
# print 
round(Consumo,1)
round(Renda,1)
#
plot(Renda,Consumo, col="blue",pch=19, ylab="Consumo (R$ 1000,00)",
            xlab="Renda (R$ 1000,00)",ylim=c(5,15))
grid()
reg = lm(Consumo~Renda)
summary(reg)
abline(reg,col="red")
#
Cest=fitted.values(reg)
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
##  Coreia de 1960 a 2021
# 
install.packages('WDI')
library(WDI)
# GDP current U$
Y=WDI(country="KOR", indicator = "NY.GDP.MKTP.CD");Y
y=Y[2:47,5]
# EXPORTS current U$
X=WDI(country="KOR", indicator = "BX.GSR.TOTL.CD");X
x=X[2:47,5]
# Transformar em bilhoes
PIB=y/(10**9)
EXPO=x/(10**9)
# Regressao linear
reg=lm(PIB~EXPO)
summary(reg)
# Elasticidade de b1 (Eb1)
coefficients(reg)[2]
# Media de EXPO
mean(EXPO)
# Media do PIB
mean(PIB)
# 
Eb1 = coefficients(reg)[2]*(mean(EXPO)/mean(PIB))
Eb1

# Grafico
plot(EXPO,PIB, col="blue",pch=19, ylab="PIB (U$ Bi)",
     xlab="EXPO (U$ Bi)")
abline(reg,col="red")
grid()
#
# Grafico usando ggplot
library("ggplot2")
dados=data.frame(cbind(PIB,EXPO))
ggplot(dados, aes(x=EXPO, y=PIB)) + 
  geom_point(col='blue') + 
  geom_smooth(method = "lm",col='red')+
  labs(x = "EXPO (U$ Bi)", y = "PIB (U$ Bi)")



