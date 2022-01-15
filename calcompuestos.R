                                 #############################################################
                                 ## Programa para hacer curvas de calibraci贸n de compuestos ##
                                 #############################################################


			                          ############################
			                          ##       Compuesto       ##
			                          ############################
			                          
			   
#pdf(file = "vai.pdf", width = 8, height = 10) #guarda la imagen en formate pdf
vai <- read.table("/Users/user/Desktop/MATLAB/vai.txt", header=FALSE, sep="", na.strings="NA", dec=".", strip.white=TRUE); vai #Carga el espectro de aobsorci贸n 
par(mfrow=c(1,2))
plot(vai[,1],vai[,2],xlim=c(245,325),ylim=c(0,1),type='l',col="green",xlab="",ylab="",  lwd = 3)
points(vai[,1],vai[,3],xlim=c(245,325),ylim=c(0,1),type='l',col="green", lwd = 3)
points(vai[,1],vai[,4],xlim=c(245,325),ylim=c(0,1),type='l',col="green", lwd = 3)
points(vai[,1],vai[,5],xlim=c(245,325),ylim=c(0,1),type='l',col="green", lwd = 3)
points(vai[,1],vai[,6],xlim=c(245,325),ylim=c(0,1),type='l',col="green", lwd = 3)
points(vai[,1],vai[,7],xlim=c(245,325),ylim=c(0,1),type='l',col="green", lwd = 3)
points(vai[,1],vai[,8],xlim=c(245,325),ylim=c(0,1),type='l',col="green", lwd = 3)
points(vai[,1],vai[,9],xlim=c(245,325),ylim=c(0,1),type='l',col="green", lwd = 3)
points(vai[,1],vai[,10],xlim=c(245,325),ylim=c(0,1),type='l',col="green", lwd = 3)
points(vai[,1],vai[,11],xlim=c(245,325),ylim=c(0,1),type='l',col="green", lwd = 3)
title(main="Curva de Calibraci髇 de la Vainillina",xlab="Longitud de Onda (nm)",ylab="Absorbancia (ua)",font.main=2,font.lab=2,cex.main=1.5)
legend(280,0.9, "sin(c x)", pch = 21, pt.bg = "white", lty = 1, col = "red")
e <- summary(vai);e
#dev.off() #salida de la grafica
####################################################  M谩ximos de Absorci贸n del compuesto ##########################################
con <- seq(0.001,0.01,0.001)
con <- matrix(con, ncol=1) #Conversi贸n a columna
con  #concentraciones
cc <- vai[,c(2,3,4,5,6,7,8,9,10,11)] #lista
cc   # solo espectros de absorci贸
mvai <- apply(cc,2,max)
mvai # M谩ximos de espectros de absorci贸n 
maxv <- matrix(mvai, ncol=1) # Conversi贸n a columna
maxv 
tv <- cbind(con, mvai)
tv
#pdf(file = "vai1.pdf", width = 8, height = 10)
plot(con,maxv,xlim=c(0.001,0.01),ylim=c(0,1.3),pch=13,col="green",xlab="",ylab="")
title(main=" Curva de Calibraci髇 Vainillina",xlab="Concentraci髇 (mg/L)",ylab="M醲imo de Absorbancia (ua)",font.main=2,font.lab=2,cex.main=1.5)
regv<-lm(maxv~con)
regv
summary(regv)
abline(a=0.004903, b=65.029152)
#dev.off()

############################################### Coeficiente de extinci贸n molar del compuesto ##############################################
xv1 <- vai[[1]];xv2 <- vai[[2]];xv3 <- vai[[3]];xv4 <- vai[[4]];xv5 <- vai[[5]]
xv6 <- vai[[6]];xv7 <- vai[[7]];xv8 <- vai[[8]];xv9 <- vai[[9]];xv10 <- vai[[10]];xv11 <- vai[[11]]

v1 <- c(xv2, xv3, xv4, xv5, xv6, xv7, xv8, xv9, xv10, xv11)
Xv <- matrix(v1,ncol=10); Xv
epv <- solve(t(con)%*%con)%*%t(Xv%*%con); epv
plot(vai[,1], epv, type='l',col='red',xlab="Longitud de Onda (nm)",ylab="M谩ximo de Absorbancia (ua)")
title(main=" Coeficiente de Absorci贸n de la Vainillina",font.main=2,font.lab=2,cex.main=1.5)
max(epv)








#pdf(file = "sir.pdf", width = 8, height = 10) #guarda la imagen en formate pdf
sir <- read.table("/Users/user/Desktop/MATLAB/sir.txt", header=FALSE, sep="", na.strings="NA", dec=".", strip.white=TRUE); sir #Carga el espectro de aobsorci贸n 
#par(mfrow=c(1,2))
plot(sir[,1],sir[,2],xlim=c(245,325),ylim=c(0,1.5),type='l',col="green",xlab="",ylab="",  lwd = 3)
legend(310,1.5, "sin(c x)", pch = 21, pt.bg = "white", lty = 1, col = "blue")
points(sir[,1],sir[,3],xlim=c(245,325),ylim=c(0,1.5),type='l',col="green", lwd = 3)
legend(310,1.5, "sin(c x)", pch = 21, pt.bg = "white", lty = 1, col = "blue")
points(sir[,1],sir[,4],xlim=c(245,325),ylim=c(0,1.5),type='l',col="green", lwd = 3)
legend(310,1.5, "sin(c x)", pch = 21, pt.bg = "white", lty = 1, col = "blue")
points(sir[,1],sir[,5],xlim=c(245,325),ylim=c(0,1.5),type='l',col="green", lwd = 3)
points(sir[,1],sir[,6],xlim=c(245,325),ylim=c(0,1.5),type='l',col="green", lwd = 3)
points(sir[,1],sir[,7],xlim=c(245,325),ylim=c(0,1.5),type='l',col="green", lwd = 3)
points(sir[,1],sir[,8],xlim=c(245,325),ylim=c(0,1.5),type='l',col="green", lwd = 3)
points(sir[,1],sir[,9],xlim=c(245,325),ylim=c(0,1.5),type='l',col="green", lwd = 3)
points(sir[,1],sir[,10],xlim=c(245,325),ylim=c(0,1.5),type='l',col="green", lwd = 3)
points(sir[,1],sir[,11],xlim=c(245,325),ylim=c(0,1.5),type='l',col="green", lwd = 3)
title(main="Curva de Calibraci贸n del Siringaldehido",xlab="Longitud de Onda (nm)",ylab="Absorbancia (ua)",font.main=2,font.lab=2,cex.main=1.5)
legend(310,1.5, "sin(c x)", pch = 21, pt.bg = "white", lty = 1, col = "red")
#dev.copy2eps()
fs <- summary(sir)
fs
#dev.off() #salida de la grafica
####################################################  M谩ximos de Absorci贸n del compuesto ##########################################
con <- seq(0.001,0.01,0.001)
con <- matrix(con, ncol=1) #Conversi贸n a columna
con  #concentraciones
cc <- sir[,c(2,3,4,5,6,7,8,9,10,11)] #lista
cc   # solo espectros de absorci贸
msir <- apply(cc,2,max)
msir # M谩ximos de espectros de absorci贸n 
maxs <- matrix(msir, ncol=1) # Conversi贸n a columna
maxs 
ts <- cbind(con, msir)
ts
#pdf(file = "sir1.pdf", width = 8, height = 10)
plot(con,maxs,,xlim=c(0.001,0.01),ylim=c(0,1.3),pch=13,col="green",xlab="",ylab="")
title(main=" Curva de Calibraci贸n del Siringaldehido",xlab="Concentraci贸n (mg/L)",ylab="M谩ximo de Absorbancia (ua)",font.main=2,font.lab=2,cex.main=1.5)
regs<-lm(maxs~con)
regs
summary(regs)
abline(a= 0.004903 , b=65.029152 )
#dev.off()

############################################### Coeficiente de extinci贸n molar del compuesto ##############################################
xs1 <- sir[[1]];xs2 <- sir[[2]];xs3 <- sir[[3]];xs4 <- sir[[4]];xs5 <- sir[[5]]
xs6 <- sir[[6]];xs7 <- sir[[7]];xs8 <- sir[[8]];xs9 <- sir[[9]];xs10 <- sir[[10]];xs11 <- sir[[11]]

si1 <- c(xs2, xs3,xs4,xs5,xs6,xs7,xs8,xs9,xs10,xs11)
Xs <- matrix(si1,ncol=10); Xs
eps <- solve(t(con)%*%con)%*%t(Xs%*%con); eps
plot(sir[,1], eps, type='l',col='red',xlab="Longitud de Onda (nm)",ylab="M谩ximo de Absorbancia (ua)")
title(main=" Coeficiente de Absorci贸n del Siringaldehido",font.main=2,font.lab=2,cex.main=1.5)
max(eps)

