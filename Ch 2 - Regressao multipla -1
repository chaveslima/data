set.seed(12)
# gerar e erros aleatorios
e1=rnorm(1:26,2)
e2=rnorm(1:26,0,2)
e3=rnorm(1:26,0,0.6)
# Gerar variavel Renda
Renda=seq(2,12,by=0.4) + e1
# Gerar variavel Preco
Preco = seq(12,8, -0.16) + e2
# Gerar variável consumo
Consumo = 4 + 0.9*Renda - 0.4*Preco + e3
# print 
round(Consumo,1)
round(Preco,1)
round(Renda,1) 
# Regressao
reg=lm(Consumo ~ Preco + Renda)
summary(reg)
### Elasticidades
Eb1 = coefficients(reg)[2]*(mean(Preco)/mean(Consumo));Eb1
Eb2 = coefficients(reg)[3]*(mean(Renda)/mean(Consumo));Eb2
#
## Em forma de Matrix ##
# definir o vetor y
y = matrix(Consumo)
# definir a Matriz X
X =  matrix(c(rep(1,26),Preco, Renda), ncol=3)
# parameters b
b=solve(t(X)%*%X)%*%t(X)%*%y ;b
#
# e = erro
e = y - X%*%b
n=nrow(y)
# Variancia do erro
S2 = (t(e)%*%e)/(n-3)
S2
# desvio padrao do erro
sqrt(S2)
