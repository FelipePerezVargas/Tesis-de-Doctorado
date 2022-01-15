rm(list=ls())
#################################################################
####                    Ajuste de Graficas                  ####
################################################################

t<-c(seq(1972,1990,2));t
ppm<-c(327.3,330.0,332.0,335.3,338.5,341.0,344.3,347.0,351.3,354.0);ppm
plot(t,ppm)

exp(-4.77)
sxy<-sum(t*ppm)

#par(bg = 'gray')
#plot(t,ppm, axes=FALSE, xlab='', ylab='', main='DiÃ³xido de Carbono en la Atmosfera',
#     cex = 1.5, col = rainbow(10),pch  = 10:20, lwd  = 3, col.axis="blue", cex.axis=1.5,
#     panel.first = grid(2, lty="dotted", lwd = 1, col='red'))
#box(col="dodgerblue")
#axis(1, col="dodgerblue", col.ticks="red", col.axis="blue", cex.axis=1)
#axis(2, col="dodgerblue", col.ticks="red", col.axis="brown", cex.axis=1)
#mtext("AÃ±o", side=1, line=3, col="red", cex=2)
#mtext("ConcentraciÃ³n de CO2 (ppm)", side=2, line=3, col="purple", cex=1.5)
sx<-sum(t)






#text(t,ppm,label=1,col='blue')
t<-c(seq(1972,1990,2));t
ppm<-c(327.3,330.0,332.0,335.3,338.5,341.0,344.3,347.0,351.3,354.0);ppm
plot(t,ppm)
minimos<-lm(ppm~t):ppm
abline(minimos[1], minimos[2], col='blue', lw=2)

########################################################################
###                      Ajuste exponencial                          ###
########################################################################

años<-c(1900, 1910, 1920, 1930, 1940, 1950, 1960, 1970, 1980, 1990, 1996);años
pob<-c(1650e6, 1750e6,1860e6, 2070e6,2300e6, 2520e6, 3020e6,3700e6,4450e6,5300e6,5770e6);pob

e1<-c(2,3,7,4,8,12)
e2<-c(5,8,8,9,10,11)
plot(e1,e2)
res<-lm(e1~e2)
res
