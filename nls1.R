library(pls);library(AppliedPredictiveModeling);library(caret);library(e1071);library(MASS);library(nls2)

rm(list=ls())
fine <- read.table("/Users/User/Desktop/MATLAB/chicos.txt", header=FALSE, sep="", na.strings="NA", dec=".", strip.white=TRUE); fine
n13 <- read.table("/Users/User/Desktop/MATLAB/Negativo12.txt", header=FALSE, sep="", na.strings="NA", dec=".", strip.white=TRUE); n13
tres <- read.table("/Users/User/Desktop/MATLAB/tres.txt", header=FALSE, sep="", na.strings="NA", dec=".", strip.white=TRUE); tres
all <- read.table("/Users/User/Desktop/MATLAB/juntos.txt", header=TRUE, sep="", na.strings="NA", dec=".", strip.white=TRUE); all

plot(fine[,1],fine[,2],xaxs="i", xaxp=c(245, 325, 8),ylim=c(0,0.6),type='l',col="blue",xlab="",ylab="", lwd=3)
points(fine[,1],fine[,3],type='l',col="black", lwd=3) 
points(fine[,1],fine[,4],type='l',col="red", lwd=3)
points(fine[,1],n13[,2],type='l',col="pink", lwd=3)

l1<-fine[25:70,1] # Longitud de onda
e1<-fine[25:70,2] ## fur
e2<-fine[25:70,3] ## 2af
e3<-fine[25:70,4] ##5mf
t13<-n13[25:70,2] # tequila n13

x<-e1+e2+e3;x
plot(l1,x, type='l', xaxp=c(245, 325, 8),ylim=c(0,2),xlab="Longitud de onda (nm)",ylab="Absorbancia (ua)")
points(l1,t13, type='l', col='red') #Tequila






n <- 50
x1 <- rnorm(n); xx1 <- scale(x1) 
x2 <- rnorm(n); xx2 <- scale(x2)
y <- x1 + x2 + rnorm(n,0,0.1); yy <- scale(y)
p <- plsr(yy ~ xx1+xx2, ncomp=1)
( w <- loading.weights(p) )



fo <- l1 ~ Const + A*x+B*x+C*x

# pass our own set of starting values
# returning result of brute force search as nls object
st1 <- expand.grid(Const = seq(0,0.01 , len = 4), B = seq(0, 2, len = 4), A = seq(0, 1, len = 4), C = seq(0, 2, len = 4))
mod1 <- nls2(fo, start = st1, algorithm = "brute-force")
mod1
# use nls object mod1 just calculated as starting value for 
# nls optimization.  Same as: nls(fo, start = coef(mod1))
nls2(fo, start = mod1) 

foo = function(t1,c,c1,c2,c3){c+e1*c1+e2*c2+e3*c3}
plot(l1,x, type='l', xaxp=c(245, 325, 8),ylim=c(0,2),xlab="Longitud de onda (nm)",ylab="Absorbancia (ua)")
curve(foo(x,0.001,4,0.7,5.9),add=T,col="red")





