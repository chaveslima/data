library(WDI)
### GDP  (constant 2015 US$) - trilhoes
Y=WDI(country="KOR", indicator = "NY.GDP.MKTP.KD");Y
y=Y[2:33,5]/(10**12)
#
### Labor people 15 ou mais - milhoes 
L=WDI(country="KOR", indicator = "SL.TLF.TOTL.IN"); L
l=L[2:33,5]/(10**6)
#
### Gross capital formation (Constante 2015 US$) - bilhoes
K=WDI(country="KOR", indicator = "NE.GDI.TOTL.KD");K
k=K[2:33,5]/(10**12)
#
# Modelo Linear
reg=lm(y~l+k)
summary(reg)
#
## PIB em Bilhões (10**9)
y=Y[2:33,5]/(10**9)
l=L[2:33,5]/(10**6)
k=K[2:33,5]/(10**12)
# Modelo linar com PIB em bilhões
reg=lm(y~l+k)
summary(reg)
##
y=Y[2:33,5]/(10**12)
l=L[2:33,5]/(10**6)
k=K[2:33,5]/(10**12)
# tranformacao logaritmica
ly=log(y)
ll=log(l)
lk=log(k)
# Modelo Log-log
reglog=lm(ly~ll+lk)
summary(reglog)
# Modelo log-lin
regloglin = lm(ly~l+k)
summary(regloglin)
# Modelo lin-log
reglinlog = lm(y~ll+lk)
summary(reglinlog)
#
