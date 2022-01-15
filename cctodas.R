                                                  ############################
			                          ##         Furfural                          ##
			                          ############################


rm(list=ls())
furan<-vector()
#pdf(file = "calfurfural.pdf", width = 8, height = 10)
f1<-for(i in 1:81){
#pdf(file = "calfurfural.pdf", width = 8, height = 10)
con <- seq(1,10,1)
fur <- read.table("/Users/User/Desktop/MATLAB/fu1.txt", header=FALSE, sep="", na.strings="NA", dec=".", strip.white=TRUE); fur
c1<-c(fur[i,2:11])
c1<-as.numeric(c1)
Rf <- cor(con, c1); Rf
regf<-lm(c1~con); regf
round(coefficients(regf)[2],4)
summary(regf)
coefficients(regf)
plot(con,t(c1),xlim=c(0,10),ylim=c(0,max(c1)),pch=11,col="blue", lwd=3, at=NULL,xlab="Concentration (mg/L)",ylab="Absortion (ua)")
title(main=" Calibration Curve of  Furfural",font.main=2,font.lab=2,cex.main=1.5)
legend(0,max(c1), "fit of  Furfural",  pt.bg = "white", lty = 1, col = "red")
legend(7, min(round(coefficients(regf)[2], 4)), as.expression(substitute(R^2==r, list(r=round(Rf,4)))))
mtext(bquote( y == .(round(coefficients(regf)[2], 4)) * x + .( round(coefficients(regf)[1], 4))), side=3, line=0, col='red',adj=0, padj=0)
abline(round(coef(regf)[1:2],4), lwd=2, col="red")
furan[i]<-print(paste( (round((coefficients(regf)[2]),4))))}
#dev.off()

############################# Coeficiente de Extinción Molar Furfural ###################################

#pdf(file = "efurfural.pdf", width = 8, height = 10)
plot(fur[,1],furan, type='l', col='blue', lwd=3)
title(main=" Extintion Molar Coefficients of Furfural",xlab="Wavelength (nm)",ylab="Absortion (ua)",font.main=2,font.lab=2,cex.main=1.5)
fine <- read.table("/Users/User/Desktop/MATLAB/chicos.txt", header=FALSE, sep="", na.strings="NA", dec=".", strip.white=TRUE); fine
#dev.off()

#################### Comparación de Corficientes de Extinción Molar  Furfural ##############################

#pdf(file = "diffurfural.pdf", width = 8, height = 10)
e1<-as.vector(fine[["V2"]]);sss<-matrix(c(furan,e1),ncol=2);a1<-sss[,1];c2<-sss[,2];
a1<-as.numeric(a1)
c2<-as.numeric(c2)
plot(fur[,1],c2-a1,ylim=c(0,max(a1)),type='l', col='yellow',lwd=3) #Diferencia de métodos
title(main=" Extintion Molar Coefficients of Furfural",xlab="Wavelength",ylab="Absortion (ua)",font.main=2,font.lab=2,cex.main=1.5)
points(fur[,1],fine[,2],type='l', col='purple',lwd=3) # Calculado con el máximo
points(fur[,1],furan, col='blue',type='l',lwd=3) #Barrido 
#dev.off()

                                                   ############################
			                           ##         2 Acetil Furfural               ##
			                          ############################



fur2<-vector()
pdf(file = "cal2furfural.pdf", width = 8, height = 10)
f2<-for(i in 1:81){
con <- seq(1,10,1)
fu2 <- read.table("/Users/User/Desktop/MATLAB/2af.txt", header=FALSE, sep="", na.strings="NA", dec=".", strip.white=TRUE); fu2
c3<-c(fu2[i,2:11])
c3<-as.numeric(c3)
R2f <- cor(con, c3); R2f
reg2f<-lm(c3~con); reg2f
round(coefficients(reg2f)[2],4)
summary(reg2f)
coefficients(reg2f)
plot(con,t(c3),xlim=c(0,10),ylim=c(0,max(c3)),pch=11,col="blue",xlab="",ylab="", lwd=3, at=NULL)
title(main=" Calibration Curve of 2 Acetyl-Furfural",xlab="Concentration (mg/L)",ylab="Absortion (ua)",font.main=2,font.lab=2,cex.main=1.5)
legend(0,max(c3), "fit of 2 Acetyl Furfural",  pt.bg = "white", lty = 1, col = "red")
legend(7, min(round(coefficients(reg2f)[2], 4)), as.expression(substitute(R^2==r, list(r=round(R2f,4)))))
mtext(bquote( y == .(round(coefficients(reg2f)[2], 4)) * x + .( round(coefficients(reg2f)[1], 4))), side=3, line=0, col='red',adj=0, padj=0)
abline(round(coef(reg2f)[1:2],4), lwd=2, col="red")
fur2[i]<-print(paste( (round((coefficients(reg2f)[2]),4))))}
dev.off()

######################## Coeficiente de Extinción Molar  2 Actil Furfural ###################################
pdf(file = "e2furfural.pdf", width = 8, height = 10)
plot(fu2[,1],fur2, type='l', col='black', lwd=3)
title(main=" Extintion Molar Coefficients of Furfural",xlab="Wavelength (nm)",ylab="Absortion (ua)",font.main=2,font.lab=2,cex.main=1.5)
fine <- read.table("/Users/User/Desktop/MATLAB/chicos.txt", header=FALSE, sep="", na.strings="NA", dec=".", strip.white=TRUE); fine
dev.off()
#################### Comparación de Corficientes de Extinción Molar  2 Acetil Furfural ########################

pdf(file = "dif2furfural.pdf", width = 8, height = 10)
e2<-as.vector(fine[["V3"]]);sss<-matrix(c(fur2,e2),ncol=2);a2<-sss[,1];c4<-sss[,2];
a2<-as.numeric(a2)
c4<-as.numeric(c4)
plot(fur[,1],c4-a2,ylim=c(0,max(a2)),type='l', col='yellow',lwd=3)
title(main=" Extintion Molar Coefficients of Furfural",xlab="Wavelength",ylab="Absortion (ua)",font.main=2,font.lab=2,cex.main=1.5)
points(fur[,1],fine[,3],type='l', col='purple',lwd=3)
points(fur[,1],fur2, col='black',type='l',lwd=3)
dev.off()


                                                   ############################
			                           ##         5 Metil Furfural                ##
			                          ############################

                          
fur5<-vector()
pdf(file = "cal5furfural.pdf", width = 8, height = 10)
f3<-for(i in 1:81){
con <- seq(1,10,1)
fu3 <- read.table("/Users/User/Desktop/MATLAB/5mf.txt", header=FALSE, sep="", na.strings="NA", dec=".", strip.white=TRUE); fu3
c5<-c(fu3[i,2:11])
c5<-as.numeric(c5)
R5f <- cor(con, c5); R5f
reg5f<-lm(c5~con); reg5f
round(coefficients(reg5f)[2],4)
summary(reg5f)
coefficients(reg5f)
plot(con,t(c5),xlim=c(0,10),ylim=c(0,max(c5)),pch=11,col="blue",xlab="",ylab="", lwd=3, at=NULL)
title(main=" Calibration Curve of 2 Acetyl-Furfural",xlab="Concentration (mg/L)",ylab="Absortion (ua)",font.main=2,font.lab=2,cex.main=1.5)
legend(0,max(c5), "fit of 2 Acetyl Furfural",  pt.bg = "white", lty = 1, col = "red")
legend(7, min(round(coefficients(reg5f)[2], 4)), as.expression(substitute(R^2==r, list(r=round(R5f,4)))))
mtext(bquote( y == .(round(coefficients(reg5f)[2], 4)) * x + .( round(coefficients(reg5f)[1], 4))), side=3, line=0, col='red',adj=0, padj=0)
abline(round(coef(reg5f)[1:2],4), lwd=2, col="red")
fur5[i]<-print(paste( (round((coefficients(reg5f)[2]),4))))}
dev.off()

######################### Coeficiente de Extinción Molar 5 Metil Furfural  ##################################

pdf(file = "e5furfural.pdf", width = 8, height = 10)
plot(fu3[,1],fur5, type='l', col='red', lwd=3)
title(main=" Extintion Molar Coefficients of Furfural",xlab="Wavelength (nm)",ylab="Absortion (ua)",font.main=2,font.lab=2,cex.main=1.5)
fine <- read.table("/Users/User/Desktop/MATLAB/chicos.txt", header=FALSE, sep="", na.strings="NA", dec=".", strip.white=TRUE); fine
dev.off()

#################### Comparación de Corficientes de Extinción Molar  5 Metil Furfural  #########################

pdf(file = "dif5furfural.pdf", width = 8, height = 10)
e3<-as.vector(fine[["V4"]]);sss<-matrix(c(fur5,e3),ncol=2);a3<-sss[,1];c6<-sss[,2];
a3<-as.numeric(a3)
c6<-as.numeric(c6)
plot(fur[,1],c6-a3,ylim=c(0,max(a3)),type='l', col='yellow',lwd=3)
title(main=" Extintion Molar Coefficients of Furfural",xlab="Wavelength",ylab="Absortion (ua)",font.main=2,font.lab=2,cex.main=1.5)
points(fur[,1],fine[,4],type='l', col='purple',lwd=3)
points(fur[,1],fur5, col='red',type='l',lwd=3)
dev.off()

#################################################################################################
et<-matrix(c(furan, fur2, fur5), ncol=3)
et
e1<-as.vector(furan)

library(pls)


fine <- read.table("/Users/User/Desktop/MATLAB/chicos.txt", header=FALSE, sep="", na.strings="NA", dec=".", strip.white=TRUE); fine
n13 <- read.table("/Users/User/Desktop/MATLAB/negativo13.txt", header=FALSE, sep="", na.strings="NA", dec=".", strip.white=TRUE); n13
tres <- read.table("/Users/User/Desktop/MATLAB/enuevos.txt", header=FALSE, sep="", na.strings="NA", dec=".", strip.white=TRUE); tres
all <- read.table("/Users/User/Desktop/MATLAB/juntos.txt", header=TRUE, sep="", na.strings="NA", dec=".", strip.white=TRUE); all

plot(fine[,1],furan,xaxs="i", xaxp=c(245, 325, 8),ylim=c(0,2),type='l',col="blue",xlab="",ylab="", lwd=3)
points(fine[,1],fur2,type='l',col="black", lwd=3) 
points(fine[,1],fur5,type='l',col="red", lwd=3)
points(fine[,1],n13[,2],type='l',col="pink", lwd=3)

l1<-fine[25:70,1] # Longitud de onda
e1<-tres[25:70,1] ## fur
e2<-tres[25:70,2] ## 2af
e3<-tres[25:70,3] ##5mf
t13<-n13[25:70,2] # tequila n13


#aj1<-t( e2+e1+e3); aj1
#e1<-fine[25:70,2] ## fur
#e2<-fine[25:70,3] ## 2af
#e3<-fine[25:70,4] ##5mf



#pls1 <- plsr(teq ~ e2+e3+e4,  data = all, validation = "CV", ncomp=3)
#pls2 <- pcr(teq ~ e2+e3+e4,  data = all, validation = "CV", ncomp=3)
#pls3 <- cppls(teq ~ e2+e3+e4,  data = all, validation = "CV", ncomp=3)
#summary(pls1)
#summary(pls1)
#summary(pls1)

#f1<-plsr()


f1 <- plsr(t13~e1+e2+e3+e1:e2, ncomp=3,validation="LOO");f1
summary(f1)
print(f1)



#max(t13)
#pdf(file = "TB16.pdf", width = 8, height = 10)

plot(l1,t13,xaxs="i", xlab='Longitud de Onda (nm)', ylab='Absorbancia (ua)' ,xaxp=c(245, 325,8),ylim=c(0,2),type='l',col="purple", lwd=3)#Muestra
points(l1,e1*coefficients(f1)[1],type='l',col="blue", lwd=3)#Furfural
points(l1,e2*coefficients(f1)[2],type='l',col="black", lwd=3)#2AF
points(l1,e3*coefficients(f1)[3],type='l',col="red", lwd=3)#5MF
points(l1,e1*coefficients(f1)[1]+e2*coefficients(f1)[2]+e3*coefficients(f1)[3],type='l',col="pink", lwd=3)#Ajuste
afur<-max(e1*coefficients(f1)[1]);afur
a2f<-max(e2*coefficients(f1)[2]);a2f
a5f<-max(e3*coefficients(f1)[3]);a5f
cof<-(afur-0.166)/max(e1);cof
co2f<-(a2f+0.0002447)/max(e2);co2f
co5f<-(a5f+0.033)/max(e3);co5f
#dev.off()