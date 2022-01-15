rm(list=ls())
library(ggplot2)
library(plotly)
library(ggthemes)
library(extrafont)
library(plyr)
library(gridExtra)

#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%##%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#
#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#% FURFURAL %#%#%#%%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%
#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%##%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#

con<-seq(1:10)
fur<-vector()
#pdf(file = "confur.pdf", width = 8, height = 10)
fur <- read.table("/Users/Admin/Downloads/Respaldo_recuperación_tesis/nfur1.txt", header=TRUE, sep="", na.strings="NA", dec=".", strip.white=TRUE)
p<-ggplot(fur, aes(x=nm, y = value, color = Concentración)) + 
  geom_line(aes(y = c1, col = "1 ppm")) + 
  geom_line(aes(y = c2, col = "2 ppm")) +
  geom_line(aes(y = c3, col = "3 ppm")) +
  geom_line(aes(y = c4, col = "4 ppm")) +
  geom_line(aes(y = c5, col = "5 ppm")) +
  geom_line(aes(y = c6, col = "6 ppm")) +
  geom_line(aes(y = c7, col = "7 ppm")) +
  geom_line(aes(y = c8, col = "8 ppm")) +
  geom_line(aes(y = c9, col = "9 ppm")) +
  geom_line(aes(y = c10, col = "10 ppm "))+
  ggtitle('Espectro de Abosrción de Furfural')+
  theme( axis.line = element_line(colour = "darkblue",size = 2, linetype = "solid"))+
  scale_x_discrete(name ="Longitud de Onda (nm)", limits=seq(245,325,10))+
  scale_y_continuous(name="Absorbancia (UA)", limits=c(0, 2))
p <- ggplotly(p);p
#dev.off()


furan<-vector()
pdf(file = "calibracionfurfural.pdf", width = 8, height = 10)
par(mfrow=c(5,2))
for(i in 2:161){
cf<-c(fur[i,2:11])
cf<-as.numeric(cf)
Rf <- cor(con, cf)
regf<-lm(cf~con)
round(coefficients(regf),4)
plot(con,t(cf),xlim=c(0,10),ylim=c(0,max(cf)),pch=21, cex=2, col="blue", bg="yellow", lwd=3, at=NULL,xlab="Concentración (ppm)",ylab="Absorción (ua)")
title(main=paste("Calibración Furfural a", 244.5+0.5*i,'(nm)'),font.main=2,font.lab=2,cex.main=1.5)
legend(0,max(cf), "Curva de Ajuste de  Furfural",  pt.bg = "gray", lty = 1, col = "blueviolet")
abline(round(coef(regf)[1:2],4), lwd=2, col="blueviolet")
legend(6, max(cf)/2.5, as.expression(substitute(R^2==r, list(r=round(Rf,4)))))
mtext(bquote( y == .(round(coefficients(regf)[2], 4)) * x + .( round(coefficients(regf)[1], 4))), side=3, line=0, col='red',adj=0, padj=0)
furan[i]<-print(paste( (round((coefficients(regf)[2]),4))))
}
dev.off() 


#pdf(file = "efurfural.pdf", width = 8, height = 10)
lon<-fur[,1]
furan<-as.numeric(furan)
lon<-as.numeric(lon)

gg2<-data.frame(lon, furan)
g1<-ggplot(gg2, aes(x=lon, y = furan))+
  geom_area(aes(y = furan),color='blue',fill = 'dodgerblue2', size=1.1) +
   ggtitle('Coeficiente de Abosrción de Furfural')+
  theme( axis.line = element_line(colour = "darkblue",size = 2, linetype = "solid"))+
  scale_x_discrete(name ="Longitud de Onda (nm)", limits=seq(245,325,10))+
  scale_y_continuous(name="Absorbancia (UA)", limits=c(0, max(furan)))
g1<-ggplotly(g1);g1
#dev.off()

hplc <- read.table("/Users/Admin/Downloads/Respaldo_recuperación_tesis/calhplc.txt", header=TRUE, sep="", na.strings="NA", dec=".", strip.white=TRUE)

fit <- lm(fu2 ~ co, data = hplc)
blue.bold.italic.16.text <- element_text(face = "bold.italic", color = "blue", size = 16)
red.bold.italic.text <- element_text(face = "bold.italic", color = "blue")
equation = function(x) {
  lm_coef <- list(a = round(coef(x)[1], digits = 2),
                  b = round(coef(x)[2], digits = 2),
                  r2 = round(summary(x)$r.squared, digits = 4));
  lm_eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(R)^2~"="~r2,lm_coef)
  as.character(as.expression(lm_eq));                 
}

#pdf(file = "furhplc.pdf", width = 8, height = 10)
fu <- ggplot(hplc, aes(x=co, y=fu2)) + geom_point(color="red",fill="blue",shape=21,alpha=0.5,size=4,stroke = 2) +
  geom_smooth(method=lm, col="springgreen4") +
  ggtitle("Curva de calibración Furfural ", subtitle="Cromatografía de Líquidos de Alta Resolución HPLC") +
  scale_x_continuous(name = "Concentración (ppm)") +
  scale_y_continuous(name = "Área (mUA)") +
  coord_cartesian(xlim=c(0,10), ylim=c(0, max(hplc[,3])))+
  theme(panel.background = element_rect(fill = 'gray94'),
        panel.grid.major = element_line(colour = "azure", size=1.5),
        panel.grid.minor = element_line(colour = "tomato", size=.25,linetype = "dashed"),
        panel.border = element_blank(),
        axis.line.x = element_line(colour = "blue", size=1.5,lineend = "butt"),
        axis.line.y = element_line(colour = "blue", size=1.5), 
        axis.text.x = element_text(face="bold", color="red", size=14, angle=00),
        axis.text.y = element_text(face="bold", color="red", size=14, angle=0),
        title = red.bold.italic.text, axis.title = blue.bold.italic.16.text) +
  annotate("rect", xmin = -0.3, xmax = 3.7, ymin = 205000, ymax = 221000, fill="white", colour="black") +
  annotate("text", x = 1.7, y = 215000, label = equation(fit), parse = TRUE)
fu
#dev.off()


#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%##%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#
#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%# 2 ACETIL FURAN #%#%#%#%#%%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%
#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%##%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#

af2<-vector()
af2 <- read.table("/Users/Admin/Downloads/Respaldo_recuperación_tesis/n2af1.txt", header=TRUE, sep="", na.strings="NA", dec=".", strip.white=TRUE)

p1<-ggplot(af2, aes(x=nm, y = value, color = Concentración)) + 
  geom_line(aes(y = c1, col = "1 ppm")) + 
  geom_line(aes(y = c2, col = "2 ppm")) +
  geom_line(aes(y = c3, col = "3 ppm")) +
  geom_line(aes(y = c4, col = "4 ppm")) +
  geom_line(aes(y = c5, col = "5 ppm")) +
  geom_line(aes(y = c6, col = "6 ppm")) +
  geom_line(aes(y = c7, col = "7 ppm")) +
  geom_line(aes(y = c8, col = "8 ppm")) +
  geom_line(aes(y = c9, col = "9 ppm")) +
  geom_line(aes(y = c10, col = "10 ppm "))+
  ggtitle('Espectro de Abosrción de 2 Acetil Furfural')+
  theme( axis.line = element_line(colour = "darkblue",size = 2, linetype = "solid"))+
  scale_x_discrete(name ="Longitud de Onda (nm)", limits=seq(245,325,10))+
  scale_y_continuous(name="Absorbancia (UA)", limits=c(0, 1.5))

p1 <- ggplotly(p1);p1


af<-vector()
pdf(file = "cal2af.pdf", width = 8, height = 10)
par(mfrow=c(5,2))
for(i in 1:161){
  caf<-c(af2[i,2:11])
  caf<-as.numeric(caf)
  Raf <- cor(con, caf)
  regaf<-lm(caf~con); regaf
  round(coefficients(regaf),4)
  plot(con,t(caf),xlim=c(0,10),ylim=c(0,max(caf)),pch=21, cex=2, col="red", bg="yellow", lwd=3, at=NULL,xlab="Concentración (ppm)",ylab="Absorción (ua)")
  title(main=paste("Calibración 2-Acetil Furfural a", 244.5+0.5*i,'(nm)'),font.main=2,font.lab=2,cex.main=1.5)
  legend(0,max(caf), "Ajuste de  2-Acetil Furfural",  pt.bg = "white", lty = 1, col = "deeppink4")
  abline(round(coef(regaf)[1:2],4), lwd=2, col="deeppink4")
  legend(6, max(caf)/2.5, as.expression(substitute(R^2==r, list(r=round(Raf,4)))))
  mtext(bquote( y == .(round(coefficients(regaf)[2], 4)) * x + .( round(coefficients(regaf)[1], 4))), side=3, line=0, col='red',adj=0, padj=0)
  af[i]<-print(paste( (round((coefficients(regaf)[2]),4))))
  }
dev.off() 




#pdf(file = "e2furfural.pdf", width = 8, height = 10)
af<-as.numeric(af)
lon<-as.numeric(lon)
gg3<-data.frame(lon, af)
g2<-ggplot(gg3, aes(x=lon, y = af))+
  geom_area(aes(y = af),color='red',fill = 'indianred1', size=1.1) +
  ggtitle('Coeficiente de Abosrción de 2-Acetil Furfural')+
  theme( axis.line = element_line(colour = "darkblue",size = 2, linetype = "solid"))+
  scale_x_discrete(name ="Longitud de Onda (nm)", limits=seq(245,325,10))+
  scale_y_continuous(name="Absorbancia (UA)", limits=c(0, max(af)))
g2<-ggplotly(g2);g2
#dev.off()


fit <- lm(a2 ~ co, data = hplc)
blue.bold.italic.16.text <- element_text(face = "bold.italic", color = "blue", size = 16)
red.bold.italic.text <- element_text(face = "bold.italic", color = "blue")
equation = function(x) {
  lm_coef <- list(a = round(coef(x)[1], digits = 2),
                  b = round(coef(x)[2], digits = 2),
                  r2 = round(summary(x)$r.squared, digits = 6));
  lm_eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(R)^2~"="~r2,lm_coef)
  as.character(as.expression(lm_eq));                 
}

a2 <- ggplot(hplc, aes(x=co, y=a2)) + 
  geom_point(color="red",fill="blue",shape=21,alpha=0.5,size=4,stroke = 2) +
  geom_smooth(method=lm, col="springgreen4") +
  ggtitle("Curva de calibración 2 Acetil Furfural ", subtitle="Cromatografía de Líquidos de Alta Resolución HPLC") +
  scale_x_continuous(name = "Concentración (ppm)") +
  scale_y_continuous(name = "Área (mUA)") +
  coord_cartesian(xlim=c(0,10), ylim=c(0, max(hplc[,4])))+
  theme(panel.background = element_rect(fill = 'gray94'),
        panel.grid.major = element_line(colour = "azure", size=1.5),
        panel.grid.minor = element_line(colour = "tomato", size=.25,linetype = "dashed"),
        panel.border = element_blank(),
        axis.line.x = element_line(colour = "blue", size=1.5,lineend = "butt"),
        axis.line.y = element_line(colour = "blue", size=1.5), 
        axis.text.x = element_text(face="bold", color="red", size=14, angle=00),
        axis.text.y = element_text(face="bold", color="red", size=14, angle=0),
        title = red.bold.italic.text, axis.title = blue.bold.italic.16.text) +
  annotate("rect", xmin = -0.3, xmax = 3.7, ymin = 150000, ymax = 170000, fill="white", colour="black") +
  annotate("text", x = 1.7, y = 160000, label = equation(fit), parse = TRUE)
a2


#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%##%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#
#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%# 5 METIL FURAN #%%#%#%#%#%%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%
#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%##%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#



mf<-vector()
mf <- read.table("/Users/Admin/Downloads/Respaldo_recuperación_tesis/n5mf1.txt", header=TRUE, sep="", na.strings="NA", dec=".", strip.white=TRUE)

p2<-ggplot(mf, aes(x=nm, y = value, color = Concentración)) + 
  geom_line(aes(y = c1, col = "1 ppm")) + 
  geom_line(aes(y = c2, col = "2 ppm")) +
  geom_line(aes(y = c3, col = "3 ppm")) +
  geom_line(aes(y = c4, col = "4 ppm")) +
  geom_line(aes(y = c5, col = "5 ppm")) +
  geom_line(aes(y = c6, col = "6 ppm")) +
  geom_line(aes(y = c7, col = "7 ppm")) +
  geom_line(aes(y = c8, col = "8 ppm")) +
  geom_line(aes(y = c9, col = "9 ppm")) +
  geom_line(aes(y = c10, col = "10 ppm "))+
  ggtitle('Espectro de Abosrción de 5-Metil Furfural')+
  theme( axis.line = element_line(colour = "darkblue",size = 2, linetype = "solid"))+
  scale_x_discrete(name ="Longitud de Onda (nm)", limits=seq(245,325,10))+
  scale_y_continuous(name="Absorbancia (UA)", limits=c(0, 2))

p2 <- ggplotly(p2);p2



me<-vector()
pdf(file = "calmf.pdf", width = 8, height = 10)
par(mfrow=c(5,2))
for(i in 1:161){
  cmf<-c(mf[i,2:11])
  cmf<-as.numeric(cmf)
  Rmf <- cor(con, cmf)
  regmf<-lm(cmf~con); regmf
  round(coefficients(regmf),4)
  plot(con,t(cmf),xlim=c(0,10),ylim=c(0,max(cmf)),pch=21, cex=2, col="green", bg="yellow", lwd=3, at=NULL,xlab="Concentración (ppm)",ylab="Absorción (ua)")
  title(paste("Calibración Furfural a", 244.5+0.5*i,'(nm)'),font.main=2,font.lab=2,cex.main=1.5)
  legend(0,max(cmf), "Ajuste del 5-Metil  Furfural",  pt.bg = "white", lty = 1, col = "coral4")
  abline(round(coef(regmf)[1:2],4), lwd=2, col="coral4")
  legend(6, max(cmf)/2.5, as.expression(substitute(R^2==r, list(r=round(Rmf,4)))))
  mtext(bquote( y == .(round(coefficients(regmf)[2], 4)) * x + .( round(coefficients(regmf)[1], 4))), side=3, line=0, col='red',adj=0, padj=0)
  me[i]<-print(paste( (round((coefficients(regmf)[2]),4))))
  }
dev.off() 


#pdf(file = "emfurfural.pdf", width = 8, height = 10)
me<-as.numeric(me)
gg4<-data.frame(lon, me)
g3<-ggplot(gg4, aes(x=lon, y = me))+
  geom_area(aes(y = me),color='green',fill = 'darkseagreen2', size=1.1) +
  ggtitle('Coeficiente de Abosrción 5-Metil Furfural')+
  theme( axis.line = element_line(colour = "darkblue",size = 2, linetype = "solid"))+
  scale_x_discrete(name ="Longitud de Onda (nm)", limits=seq(245,325,10))+
  scale_y_continuous(name="Absorbancia (UA)", limits=c(0, max(me)))
g3<-ggplotly(g3);g3
#dev.off()


tf<-vector()
tf <- read.table("/Users/User/Desktop/porfavor/tf.txt", header=TRUE, sep="", na.strings="NA", dec=".", strip.white=TRUE)



fit <- lm(m5 ~ co, data = hplc)
blue.bold.italic.16.text <- element_text(face = "bold.italic", color = "blue", size = 16)
red.bold.italic.text <- element_text(face = "bold.italic", color = "blue")
equation = function(x) {
  lm_coef <- list(a = round(coef(x)[1], digits = 2),
                  b = round(coef(x)[2], digits = 2),
                  r2 = round(summary(x)$r.squared, digits = 4));
  lm_eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(R)^2~"="~r2,lm_coef)
  as.character(as.expression(lm_eq));                 
}

#pdf(file = "hmfhplc.pdf", width = 8, height = 10)
m5 <- ggplot(hplc, aes(x=co, y=m5)) + geom_point(color="red",fill="blue",shape=21,alpha=0.5,size=4,stroke = 2) +
  geom_smooth(method=lm, col="springgreen4") + 
  ggtitle("Curva de calibración 5 Metil Furfural ", subtitle="Cromatografía de Líquidos de Alta Resolución HPLC") +
  scale_x_continuous(name = "Concentración (ppm)") +
  scale_y_continuous(name = "Área (mUA)") +
  coord_cartesian(xlim=c(0,10), ylim=c(0, max(hplc[,5])))+
  theme(panel.background = element_rect(fill = 'gray94'),
        panel.grid.major = element_line(colour = "azure", size=1.5),
        panel.grid.minor = element_line(colour = "tomato", size=.25,linetype = "dashed"),
        panel.border = element_blank(),
        axis.line.x = element_line(colour = "blue", size=1.5,lineend = "butt"),
        axis.line.y = element_line(colour = "blue", size=1.5), 
        axis.text.x = element_text(face="bold", color="red", size=14, angle=00),
        axis.text.y = element_text(face="bold", color="red", size=14, angle=0),
        title = red.bold.italic.text, axis.title = blue.bold.italic.16.text) +
  annotate("rect", xmin = -0.3, xmax = 3.7, ymin = 205000, ymax = 221000, fill="white", colour="black") +
  annotate("text", x = 1.7, y = 215000, label = equation(fit), parse = TRUE)
m5

########################### Todos los coeficiente de absorción molar ############################
g4<-ggplot(gg4, aes(x=lon, y = furan))+
  geom_area(aes(y =furan ),color='blue',fill = 'dodgerblue2', size=1.1, alpha = 0.5) +
  geom_area(aes(y = af),color='red',fill = 'indianred1', size=1.1, alpha = 0.7) +
  geom_area(aes(y = me),color='green',fill = 'darkseagreen2', size=1.1, alpha = 0.9) +
  ggtitle('Coeficientes de Absorción Molar Furfurales')+
  theme( axis.line = element_line(colour = "darkblue",size = 2, linetype = "solid"))+
  scale_x_discrete(name ="Longitud de Onda (nm)", limits=seq(245,325,10))+
  scale_y_continuous(name="Absorbancia (UA)", limits=c(0, max(furan)))+
  annotate("text", x =277, y = 0.13, label ='Furfural', colour='blue')+
  annotate("text", x =275, y = 0.08, label ='2A-Furfural', colour='red')+
  annotate("text", x =276, y = 0.03, label ='5M-Furfural', colour='darkseagreen4')
  geom_vline(xintercept = 285, color = "red", linetype = "dashed") 
g4<-ggplotly(g4);g4



########################### Todos los coeficiente de absorción molar Acotados ############################
g5<-ggplot(gg4, aes(x=lon, y = furan))+
  geom_area(aes(y =furan ),color='blue',fill = 'dodgerblue2', size=1.1, alpha = 0.5) +
  geom_area(aes(y = af),color='red',fill = 'indianred1', size=1.1, alpha = 0.7) +
  geom_area(aes(y = me),color='green',fill = 'darkseagreen2', size=1.1, alpha = 0.9) +
  ggtitle('Coeficientes de Absorción Molar Furfurales')+
  theme( axis.line = element_line(colour = "darkblue",size = 2, linetype = "solid"))+
  scale_x_discrete(name ="Longitud de Onda (nm)", limits=seq(245,325,10))+
  scale_y_continuous(name="Absorbancia (UA)", limits=c(0, max(furan)))+
  annotate("text", x =277, y = 0.13, label ='Furfural', colour='blue')+
  annotate("text", x =275, y = 0.08, label ='2A-Furfural', colour='red')+
  annotate("text", x =276, y = 0.03, label ='5M-Furfural', colour='darkseagreen4')+
  geom_vline(xintercept = 265, color = "darkblue", linetype = "dashed")+
  geom_vline(xintercept = 305, color = "darkblue", linetype = "dashed")
g5<-ggplotly(g5);g5


##################################################################################################
######################################## Tequilas ###############################################
#################################################################################################
tequilas <- read.table("/Users/Admin/Downloads/Respaldo_recuperación_tesis/utequilas.txt", header=FALSE, sep="", na.strings="NA", dec=".", strip.white=TRUE)

names<-c('l_onda',	'25-espolon',	'27-espolon',	'5-sm	22-arette',	'2-pueblo-viejo',	'3-san-matias',	'9-tres alegres',	'26-espolon', '15-don-eduardo',
'24-espolon',	'7-sm',	'17-herradura1',	'11-quiote',	 '10-centenario-plata',	'b37',	'26',	'28',	'16',	'17',	'mh1',	'mh2',	'mh3',	
'blanco55',	'blanco54',	'28a',	'b38',	'n4',	'n5',	'n5a',	'n9',	'p6a',	'n7',	'n12',	'n8',	'n11a',	'n13',
'n10',	'n6',	'p13',	'p12',	'p5',	'p7',	'p8a',	'p9',	'p6',	'p8',	'p4')

#tequilas<-vector()
#pdf(file = "tequilas.pdf", width = 8, height = 10)
#par(mfrow=c(5,2))
 for(i in 2:48){
pt<-ggplot(tequilas, aes(x=V1, y =tequilas[,i], color = V1)) + 
    geom_line(aes(y = tequilas[,i], col =paste('jhgjhg')))
pt[i]<-ggplotly(pt);pt
  }
#dev.off()

par(mfrow=c(2,2))
for(i in 2:48){
plot(tequilas[,1], tequilas[,i], xlab='Longitud de Onda (nm)', ylab = 'Espectro de Absorción Molar (ua)')
}

library(RColorBrewer)

plot(tequilas[,1], tequilas[,2], type='l', lwd=2.5, xlab="Longitud de Onda (nm)",ylab="Absorción (ua)", ylim =c(0,3))
for(i in 3:length(tequilas)){
lines(tequilas[,1], tequilas[,i], type='l', lwd=2.5, col= c(1:length(tequilas)))  
}


for (i in 3:length(tequilas)) { 
  print(ggplot(tequilas,aes(tequilas[,1],tequilas[,2]))+geom_line(y=tequilas[,i])) 
}


display.brewer.all()
n <- 48
plot(1:n, pch=CIRCLE<-16, cex=1:n, col=rainbow(n))

tequilasr <- read.table("/Users/User/Desktop/porfavor/rtnr.txt", header=FALSE, sep="", na.strings="NA", dec=".", strip.white=TRUE)
er <- read.table("/Users/User/Desktop/porfavor/re.txt", header=FALSE, sep="", na.strings="NA", dec=".", strip.white=TRUE)


plot(tequilasr[,1], tequilasr[,25], type = 'line', ylim = c(0, max(tequilasr[,25])))
lines(tequilasr[,1], .9*er[,2], col='blue')
lines(tequilasr[,1],.279*er[,3], col='red')
lines(tequilasr[,1],.257*er[,4], col='green')
lines(tequilasr[,1], (.9*er[,2]+.279*er[,3]+.257*er[,4]), col='yellow', lwd='3')
abline(h=0, col='darkslateblue')

plot(er[,1], er[,2])



con<-seq(1:10)
sir<-vector()
#pdf(file = "confur.pdf", width = 8, height = 10)
sir <- read.table("/Users/Admin/Downloads/Respaldo_recuperación_tesis/sir.txt", header=TRUE, sep="", na.strings="NA", dec=".", strip.white=TRUE)
p6<-ggplot(sir, aes(x=nm, y = value, color = Concentración)) + 
  geom_line(aes(y = c1, col = "1 ppm")) + 
  geom_line(aes(y = c2, col = "2 ppm")) +
  geom_line(aes(y = c3, col = "3 ppm")) +
  geom_line(aes(y = c4, col = "4 ppm")) +
  geom_line(aes(y = c5, col = "5 ppm")) +
  geom_line(aes(y = c6, col = "6 ppm")) +
  geom_line(aes(y = c7, col = "7 ppm")) +
  geom_line(aes(y = c8, col = "8 ppm")) +
  geom_line(aes(y = c9, col = "9 ppm")) +
  geom_line(aes(y = c10, col = "10 ppm "))+
  ggtitle('Espectro de Abosrción del Siringaldehido')+
  theme( axis.line = element_line(colour = "darkblue",size = 2, linetype = "solid"))+
  scale_x_discrete(name ="Longitud de Onda (nm)", limits=seq(245,325,10))+
  scale_y_continuous(name="Absorbancia (UA)", limits=c(0, 0.75))
p6 <- ggplotly(p6);p6
#dev.off()



con<-seq(1:10)
vai<-vector()
#pdf(file = "vainillina.pdf", width = 8, height = 10)
vai <- read.table("/Users/Admin/Downloads/Respaldo_recuperación_tesis/vai.txt", header=TRUE, sep="", na.strings="NA", dec=".", strip.white=TRUE)
p7<-ggplot(vai, aes(x=nm, y = value, color = Concentración)) + 
  geom_line(aes(y = c1, col = "1 ppm")) + 
  geom_line(aes(y = c2, col = "2 ppm")) +
  geom_line(aes(y = c3, col = "3 ppm")) +
  geom_line(aes(y = c4, col = "4 ppm")) +
  geom_line(aes(y = c5, col = "5 ppm")) +
  geom_line(aes(y = c6, col = "6 ppm")) +
  geom_line(aes(y = c7, col = "7 ppm")) +
  geom_line(aes(y = c8, col = "8 ppm")) +
  geom_line(aes(y = c9, col = "9 ppm")) +
  geom_line(aes(y = c10, col = "10 ppm "))+
  ggtitle('Espectro de Abosrción del Siringaldehido')+
  theme( axis.line = element_line(colour = "darkblue",size = 2, linetype = "solid"))+
  scale_x_discrete(name ="Longitud de Onda (nm)", limits=seq(245,325,10))+
  scale_y_continuous(name="Absorbancia (UA)", limits=c(0, 0.75))
p7 <- ggplotly(p7);p7
#dev.off()

av<-vector()
pdf(file = "vainillina.pdf", width = 8, height = 10)
par(mfrow=c(5,2))
for(i in 1:161){
  cav<-c(vai[i,2:11])
  cav<-as.numeric(cav)
  con<-as.numeric(con)
  Rav <- cor(con, cav)
  regav<-lm(cav~con); regav
  round(coefficients(regav),4)
  plot(con,t(cav),xlim=c(0,10),ylim=c(0,max(cav)),pch=21, cex=2, col="red", bg="yellow", lwd=3, at=NULL,xlab="Concentración (ppm)",ylab="Absorción (ua)")
  title(main=paste("Calibración Vainillina a", 244.5+0.5*i,'(nm)'),font.main=2,font.lab=2,cex.main=1.5)
  legend(0,max(cav), "Ajuste de  Vainillina",  pt.bg = "white", lty = 1, col = "deeppink4")
  abline(round(coef(regav)[1:2],4), lwd=2, col="deeppink4")
  legend(6, max(cav)/2.5, as.expression(substitute(R^2==r, list(r=round(Rav,4)))))
  mtext(bquote( y == .(round(coefficients(regav)[2], 4)) * x + .( round(coefficients(regav)[1], 4))), side=3, line=0, col='red',adj=0, padj=0)
  av[i]<-print(paste( (round((coefficients(regav)[2]),4))))
}
dev.off() 




p <- list()
for(i in 1:4){
  p[[i]] <- qplot(1:10,10:1,main=i)
}



