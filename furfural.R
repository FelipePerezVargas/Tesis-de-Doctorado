                                                 ############################
			                          ##         Furfural                          ##
			                          ############################

library(pls)
rm(list=ls())
furan<-vector()
intfur<-vector()
penfur<-vector()
#pdf(file = "cfur.pdf", width = 8, height = 10)
f1<-for(i in 1:81){
#pdf(file = "calfurfural.pdf", width = 8, height = 10)
con <- seq(1,10,1)
fur <- read.table("/Users/User/Desktop/MATLAB/fu1.txt", header=FALSE, sep="", na.strings="NA", dec=".", strip.white=TRUE); fur
c1<-c(fur[i,2:11])
c1<-as.numeric(c1)
Rf <- cor(con, c1); Rf
regf<-lm(c1~con); regf
intfur[i]<-round(coefficients(regf)[1],4)
penfur[i]<-round(coefficients(regf)[2],4)
#summary(regf)
#coefficients(regf)
plot(con,t(c1),xlim=c(0,10),ylim=c(0,max(c1)),pch=11,col="blue", lwd=3, at=NULL,xlab="Concentration (mg/L)",ylab="Absortion (ua)")
title(main=" Calibration Curve of  Furfural",font.main=2,font.lab=2,cex.main=1.5)
legend(0,max(c1), "fit of  Furfural",  pt.bg = "white", lty = 1, col = "red")
legend(7, min(round(coefficients(regf)[2], 4)), as.expression(substitute(R^2==r, list(r=round(Rf,4)))))
mtext(bquote( y == .(round(coefficients(regf)[2], 4)) * x + .( round(coefficients(regf)[1], 4))), side=3, line=0, col='red',adj=0, padj=0)
abline(round(coef(regf)[1:2],4), lwd=2, col="red")
furan[i]<-paste( (round((coefficients(regf)[2]),4)))}
#dev.off()

write(furan, file='efurfural.txt')
write(intfur, file='e_int_f.csv')
write(penfur, file='e_pen_f.txt')


predict(regf)
new <- data.frame(wt=c(1.15, 1.16,1.17), interval='confidence')
predict(regf, new, se.fit = TRUE)


############################# Coeficiente de Extinción Molar Furfural ###################################

#pdf(file = "efur.pdf", width = 8, height = 10)
plot(fur[,1],furan, type='l', col='blue', lwd=3,xlab="Wavelength (nm)",ylab="Absortion (ua)")
title(main=" Extintion Molar Coefficients of Furfural",font.main=2,font.lab=2,cex.main=1.5, pch=17)
points(277, max(furan), pch=17, col='blue')

#dev.off()

fine <- read.table("/Users/User/Desktop/MATLAB/chicos.txt", header=FALSE, sep="", na.strings="NA", dec=".", strip.white=TRUE); fine

#################### Comparación de Corficientes de Extinción Molar  Furfural ##############################

#pdf(file = "diffurfural.pdf", width = 8, height = 10)
e1<-as.vector(fine[["V2"]]);sss<-matrix(c(furan,e1),ncol=2);a1<-sss[,1];c2<-sss[,2];
a1<-as.numeric(a1)
c2<-as.numeric(c2)
plot(fur[,1],c2-a1,ylim=c(0,max(a1)), xlim=c(min(fur[,1]),max(fur[,1])),type='l', col='yellow',lwd=3,xlab="Wavelength",ylab="Absortion (ua)") #Diferencia de métodos
title(main=" Extintion Molar Coefficients of Furfural",font.main=2,font.lab=2,cex.main=1.5)
points(fur[,1],fine[,2],type='l', col='purple',lwd=3) # Calculado con el máximo
points(fur[,1],furan, col='blue',type='l',lwd=3) #Barrido 
#dev.off()
