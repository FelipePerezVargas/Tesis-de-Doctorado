rm(list=ls())
library(rgl)


open3d()
x <- sort(rnorm(1000))
y <- rnorm(1000)
z <- rnorm(1000) + atan2(x, y)
plot3d(x, y, z, col = rainbow(1000))

old.plt <- par("plt")
models <- c("ppl","fdp","fgn","dfbm")
for (i in seq(along=models)){
  splitplot(2,2,i)
  plot(lmSimulate(lmModel(models[i])),
       reference.grid=FALSE)
}
par(plt=old.plt)


a=-0.3;b=-0.4 # Complex parameter, connected to coordinate of the Mandelbrot set in a complex plane. Constants here.
Limits=c(-2,2)
MaxIter=6000
cl=colours()
Step=seq(Limits[1],Limits[2],by=0.01)
PointsMatrix=array(0,dim=c(length(Step)*length(Step),3))
a1=0
plot(0,0,xlim=Limits,ylim=Limits,col="white")

for(x in Step)
{
  for(y in Step)
  {
    n=0
    DIST=0
    x1=x;y1=y # Original x and y are saved.
    while(n<MaxIter & DIST<4)
    {
      newx=x1^2-y1^2+a
      newy=2*x1*y1+b
      DIST=newx^2+newy^2
      x1=newx;y1=newy
      n=n+1
    }
    if(DIST<4) colour=24 else colour=n*10
    #points(x,y, pch=".", col=cl[colour])
    a1=a1+1
    PointsMatrix[a1,]=c(x,y,colour)
  }
}

X11()
plot(PointsMatrix[,1], PointsMatrix[,2], xlim=Limits, ylim=Limits, col=cl[PointsMatrix[,3]], pch=".")



#Script written by Allan Roberts, 2012.
snowflake <- function(k=3){
  
  Rotate <- function(a){
    #returns a matrix for a rotation of a.
    A <- matrix(0,2,2);
    A[1,1] <- cos(a);
    A[1,2] <- -sin(a);
    A[2,1] <- sin(a);
    A[2,2] <- cos(a);
    return(A);
  }
  
  Reflect <- function(a){
    
    #a reflection across the line determined by angle a.
    A <- matrix(0,2,2);
    A[1,1] <- 1;
    A[2,2] <- -1;
    B <- Rotate(a) %*% A %*% Rotate(-a);
    return(B);
  }
  
  locator.with.point <- function(n){
    X <- numeric(n);
    Y <- numeric(n);
    for (i in 1:n){P<-locator(1);X[i]<-P$x;Y[i]<-P$y; points(X[i],Y[i],pch=19,col=4)}
    return(list(X,Y))
  }
  
  par( bg = 1 )
  
  X1 <- numeric(6);
  Y1 <- numeric(6);
  
  for (i in 1:6) {X1[i] <- sin(i*2*pi/6)}
  for (i in 1:6) {Y1[i] <- cos(i*2*pi/6)}
  plot.new();
  par(usr=c(-2,2,-2,2), bg=1);
  polygon(X1,Y1,col=rgb(1,1,1));
  abline(0,tan(pi/6),lty=3);
  abline(0,-tan(pi/6),lty=3);
  lines(c(0,0),c(-1,1),lty=3);
  C <- list(matrix(0,2,3),matrix(0,2,3),matrix(0,2,3));
  
  for (i in 1:k){
    X <- locator.with.point(3);
    X <- rbind(X[[1]],X[[2]]);
    C[[i]] <- X;
    polygon(t(C[[i]]),col=rgb(1,1,1,0),border=1,lty=2,cex=0.2);
  }
  
  #Draw the Snowflake
  plot.new();
  par(usr=c(-2,2,-2,2), bg=1);
  polygon(X1,Y1,col=rgb(1,1,1));
  for (j in 1:k){
    for (i in 1:6){ R <- Rotate(i*pi/3) %*% Reflect(pi/3) %*% C[[j]]; polygon(t(R), col=1)}
    for (i in 1:6){R <- Rotate(i*pi/3) %*% C[[j]]; polygon(t(R),col=1);}
  }
}
snowflake( )


IterateTriangle <- function(A){
  B <- cbind(A,0*A);
  C <- cbind(A,A);
  D <- rbind(B,C);
  return(D);
}

par(mfrow=c(2,8))
for (i in 1:16){
  T <- matrix(1,1,1)
  for (i in 1:i) T <- IterateTriangle(T);
  image(T,col=c("white","black"),axes=FALSE);
  text(0,1,i,col="black") 
}
 
