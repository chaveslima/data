library(WDI)
### GDP  (constant 2015 US$) - trilhoes
Y=WDI(country="KOR", indicator = "NY.GDP.MKTP.KD");Y
y=Y[2:33,5]/(10**12); y; plot.ts(rev(y))
#
### Labor people 15 ou mais - milhoes 
L=WDI(country="KOR", indicator = "SL.TLF.TOTL.IN"); L
l=L[2:33,5]/(10**6); l; plot.ts(rev(l))
#
### Gross capital formation (Constante 2015 US$) - bilhoes
K=WDI(country="KOR", indicator = "NE.GDI.TOTL.KD");K
k=K[2:33,5]/(10**12); plot.ts(rev(k)); k
#
reg=lm(y~l+k)
summary(reg)
#Elasticidades
Eb1 = coefficients(reg)[2]*(mean(l)/mean(y));Eb1
Eb2 = coefficients(reg)[3]*(mean(k)/mean(y));Eb2

