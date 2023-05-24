#############################
### Polinomial Cubico ###
#############################
set.seed(67)
e=rnorm(1:26,0,60)
Q=seq(0,70,length=26)
CT=200+50*Q-1.58*(Q**2)+0.02*(Q**3) + e
round(Q,0);
round(CT,0)
#
# gerar as variaveis Q2 e Q3
Q2=Q**2;Q3=Q**3
# rodar a regressao e guardar em regp3
regp3=lm(CT ~ Q + Q2 + Q3)
# mostrar os resultados
summary(regp3)
# Coeficientes Estimados
b1=coefficients(regp3)[2];b1
b2=coefficients(regp3)[3];b2
b3=coefficients(regp3)[4];b3
# Medias de Q (QM) e CT (CTM)
QM=mean(Q);CTM=mean(CT)
# Efeito Marginal (EM)
EM = b1 + 2*b2*(QM) + 3*b3*(QM**2)
EM
# Elasticidade (E)
E = EM*(QM/CTM)
E
# Plot de pontos CTxQ
plot(CT ~ Q, pch=19, col="blue") 
# Adicionar lina CTxQ estimado
lines(Q,fitted(regp3),col = "red",type = "l")
# Inserir legenda
legend(5, 2500, legend=c("CT Simulado","CT Estimado"),
      col=c("blue","red"),lty=c(0,1), pch=c(19,32),cex=0.9,box.lty=0)

#############################
### Polinomial Quadratico ###
#############################
set.seed(78)
# simulacao dos dados
e=rnorm(1:26,0,60)
Q=seq(0,70,length=26)
CMg = 2000 - 100*Q + 1.6*(Q**2) + e
round(Q)
round(CMg)
# Estimação
Q2=Q**2
regp2=lm(CMg ~ Q + Q2)
summary(regp2)
#
# Coeficientes Estimados
b1=coefficients(regp2)[2];b1
b2=coefficients(regp2)[3];b2
# Medias de Q (QM) e CMg (CMgM)
QM=mean(Q);CMgM=mean(CMg)
#
# Efeito Marginal (EM)
EM = b1 + 2*b2*(QM)
EM
# Elasticidade (E)
E = EM*(QM/CMgM)
E
# Grafico
plot(CMg ~ Q, pch=19, col="blue") 
lines(Q,fitted(regp2),col = "red",type = "l")
legend(5, 2500, legend=c("CMg Simulado","CMg Estimado"),
       col=c("blue","red"),lty=c(0,1), pch=c(19,32),cex=0.9,box.lty=0)

