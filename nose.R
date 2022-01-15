			  #/%/&%(%(&%/%&/%/&%&/%&/%&/%/%&/%&/%&/%&/%&/%&/%&/%&/%&/%&/%/&%&/%/%&/%&#
			  #                                                                       #
			  #      Programa para calcular las curvas de calibración de furfurales   #
			  #                                                                       #
			  #/&%&/%&/%&/%&/%&/%&/%&/%&/%&/%&/%&/%&/%&/%/&%&/%&/%&/%&/%&/%/&%/&%&/%/&#


			  
			  
			                          ############################
			                          ## 5 Hidroximetilfurfural ##
			                          ############################
			                          
			                          
hmf5 <- read.table("/home/rogelio/Escritorio/MATLAB/5hmf.txt", header=FALSE, sep="", na.strings="NA", dec=".", strip.white=TRUE)

plot(hmf5[,1],hmf5[,2],xlim=c(245,325),ylim=c(0,1.5),,pch=19,col="green",xlab="",ylab="")
points(hmf5[,1],hmf5[,3],xlim=c(245,325),ylim=c(0,1.5),pch=19,col="green")
points(hmf5[,1],hmf5[,4],xlim=c(245,325),ylim=c(0,1.5),pch=19,col="green")
points(hmf5[,1],hmf5[,5],xlim=c(245,325),ylim=c(0,1.5),pch=19,col="green")
points(hmf5[,1],hmf5[,6],xlim=c(245,325),ylim=c(0,1.5),pch=19,col="green")
points(hmf5[,1],hmf5[,7],xlim=c(245,325),ylim=c(0,1.5),pch=19,col="green")
points(hmf5[,1],hmf5[,8],xlim=c(245,325),ylim=c(0,1.5),pch=19,col="green")
points(hmf5[,1],hmf5[,9],xlim=c(245,325),ylim=c(0,1.5),pch=19,col="green")
points(hmf5[,1],hmf5[,10],xlim=c(245,325),ylim=c(0,1.5),pch=19,col="green")
points(hmf5[,1],hmf5[,11],xlim=c(245,325),ylim=c(0,1.5),pch=19,col="green")
#points(Furfurales[,1],Furfurales[,4],xlim=c(245,325),ylim=c(0,1.5),pch=19,col="green",xlab="",ylab="")
title(main="5 Hidroximetilfurfural",xlab="Longitud de Onda (nm)",ylab="Absorbancia (ua)",font.main=2,font.lab=2,cex.main=1.5)
a <- summary(hmf5)
a

####################################################  Máximos de Absorción 5HMF ##########################################




con <- seq(0.001,0.01,0.001)
con <- matrix(con, ncol=1) #Conversión a columna
con  #concentraciones
cc <- hmf5[,c(2,3,4,5,6,7,8,9,10,11)]
cc   # solo espectros de absorció
m5hmf <- apply(cc,2,max)
m5hmf # Máximos de espectros de absorción 
maxf <- matrix(m5hmf, ncol=1) # Conversión a columna
maxf 
tf <- cbind(con, m5hmf)
tf
plot(con,maxf,,xlim=c(0.001,0.01),ylim=c(0,1.3),,pch=19,col="green",xlab="",ylab="")
title(main=" Curva de Calibración 5 Hidroximetilfurfural",xlab="Concentración (mg/L)",ylab="Máximo de Absorbancia (ua)",font.main=2,font.lab=2,cex.main=1.5)
reg<-lm(maxf~con)
reg
abline(a=-0.003509 , b=131.846970)


library(relimp, pos=4)

