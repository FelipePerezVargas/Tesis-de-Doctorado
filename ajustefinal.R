library(pls)#;library(AppliedPredictiveModeling);library(caret);library(e1071);library(MASS)#;library(nls2)
library(chemometrics)

rm(list=ls())
fine <- read.table("/Users/User/Desktop/MATLAB/chicos.txt", header=FALSE, sep="", na.strings="NA", dec=".", strip.white=TRUE)
n13 <- read.table("/Users/User/Desktop/MATLAB/negativo13.txt", header=FALSE, sep="", na.strings="NA", dec=".", strip.white=TRUE)
tres <- read.table("/Users/User/Desktop/MATLAB/tres.txt", header=FALSE, sep="", na.strings="NA", dec=".", strip.white=TRUE); tres
#tres <- read.table("/Users/User/Desktop/MATLAB/enuevos.txt", header=FALSE, sep="", na.strings="NA", dec=".", strip.white=TRUE)
all <- read.table("/Users/User/Desktop/MATLAB/juntos.txt", header=TRUE, sep="", na.strings="NA", dec=".", strip.white=TRUE)

plot(fine[,1],fine[,2],xaxs="i", xaxp=c(245, 325, 8),ylim=c(0,2),type='l',col="blue",xlab="",ylab="", lwd=3)
points(fine[,1],fine[,3],type='l',col="black", lwd=3) 
points(fine[,1],fine[,4],type='l',col="red", lwd=3)
points(fine[,1],n13[,2],type='l',col="pink", lwd=3)

l1<-fine[32:59,1] # Longitud de onda
e1<-fine[32:59,2] ## fur
e2<-fine[32:59,3] ## 2af
e3<-fine[32:59,4] ##5mf
t13<-n13[32:59,2] # tequila n13
#aj1<-t( e2+e1+e3); aj1

#pls1 <- plsr(teq ~ e2+e3+e4,  data = all, validation = "CV", ncomp=3)
#pls2 <- pcr(teq ~ e2+e3+e4,  data = all, validation = "CV", ncomp=3)
#pls3 <- cppls(teq ~ e2+e3+e4,  data = all, validation = "CV", ncomp=3)
#summary(pls1)
#summary(pls1)
#summary(pls1)

#f1<-plsr()
f1<- plsr(t13 ~ e1+e2+e3,   validation = "CV", ncomp=3,method = "kernelpls")
#f1<- plsr(teq ~ e2+e3+e4,  data = all, validation = "LOO", ncomp=3);f1


#f2<-plsr(t13,e3+e1+e2,
#ncomp = 3,
#scale = TRUE,
#mode = c("regression", "canonical", "invariant", "classic"),
#tol = 1e-06,
#max.iter = 10,
#near.zero.var = FALSE,
#logratio="none",
#multilevel=NULL)



#f1 <- pls1_nipals(t13,e1+e2+e3, a=1, it=50, tol=1e-8, scale=FALSE);f1
summary(f1)
print(f1)
#coefficients(f1);f1
 #Other useful functions 
#confint(f1, level=0.95) # CIs for model parameters 
#fitted(f1) # predicted values
#r1<-residuals(f1) # residuals
#anova(f1) # anova table 
#vcov(f1) # covariance matrix for model parameters 
#influence(f1) # regression diagnostics
#dd<-c(t13,aj1);dd
 #diagnostic plots 
#layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page 
#plot(f1)



#max(t13)
#pdf(file = "TB16.pdf", width = 8, height = 10)
plot(l1,t13,xaxs="i",yaxs="i", xlab='Longitud de Onda (nm)', ylab='Absorbancia (ua)' ,xaxp=c(245, 325,8),ylim=c(0,max(t13)),type='l',col="purple", lwd=3)#Muestra
points(l1,e1*coefficients(f1)[1],type='l',col="blue", lwd=3)#Furfural
points(277,max(e1*coefficients(f1)[1]), col='blue', pch=11)
points(l1,e2*coefficients(f1)[2],type='l',col="black", lwd=3)#2AF
points(274,max(e2*coefficients(f1)[2]), col='black', pch=11)
points(l1,e3*coefficients(f1)[3],type='l',col="red", lwd=3)#5MF
points(293,max(e3*coefficients(f1)[3]), col='red', pch=11)
points(l1,e1*coefficients(f1)[1]+e2*coefficients(f1)[2]+e3*coefficients(f1)[3],type='l',col="pink", lwd=3)#Ajuste
afur<-max(e1*coefficients(f1)[1]);afur
a2f<-max(e2*coefficients(f1)[2]);a2f
a5f<-max(e3*coefficients(f1)[3]);a5f
cof<-(afur-0.166)/max(e1);cof
co2f<-(a2f+0.0002447)/max(e2);co2f
co5f<-(a5f+0.033)/max(e3);co5f
#dev.off()
