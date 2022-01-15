
#################################################################################################################################
## La función persp() permite construir gráficos en 3D. En particular, permite representar funciones matemáticas con facilidad.## 
## A modo de ejemplo, en esta sección mostraremos como construir el siguiente gráfico, un paraboloide de ecuación z=x2+y2      ##
#################################################################################################################################
rm(list=ls())
x=seq(-50,50,length=50)
y=seq(-50,50,length=50)
parabola=function(x,y) x^2+y^2
z=outer(x, y, parabola)
ecuacionParab <- expression(z == x^2 + y^2)
persp(x,y,z,expand = 1,theta = 30, phi=30, col="green", main="Paraboloide", sub=ecuacionParab, col.main="blue", scale=T,
      border=NA, shade=0.5)
persp(x,y,z,expand = 1,theta = 30, phi=30, col="green", main="Paraboloide", sub=ecuacionParab, col.main="blue", scale=T,
      shade=0.5)
#############################  Curvas de Nivel ###################################
image(x,y,z)
contour(x,y,z,add=T)


####################################################################################



x=seq(-5,5,length=50)
y=seq(-5,5,length=50)
parabola=function(x,y) x-y-2
z=outer(x, y, parabola)
ecuacionParab <- expression(z == x-y-2)
persp(x,y,z,expand = 1,theta = 30, phi=30, col="green", main="Plano", sub=ecuacionParab, col.main="blue", scale=T,
      border=NA, shade=0.5)
persp(x,y,z,expand = 1,theta = 30, phi=30, col="green", main="Plano", sub=ecuacionParab, col.main="blue", scale=T,
      shade=0.5)
image(x,y,z)
contour(x,y,z,add=T)


###############################################################################################

x=seq(0,2,length=10)
y=seq(0,2*pi,length=10)
a<-1; b<- -2
X<-a+x*cos(y);
Y<-b+x*sin(y);
parabola=function(X,Y) X*Y???X**2???Y**2???2*X???2*Y+4
z=outer(X, Y, parabola)
ecuacionParab <- expression(z == x*y???x**2???y*2???2*x???2*y+4)
persp(X,Y,z,expand = 1,theta = 30, phi=30, col="green", main="Curva de Nivel", sub=ecuacionParab, col.main="blue", scale=T,
      border=NA, shade=0.5)
persp(X,Y,z,expand = 1,theta = 0, phi=0, col="green", main="Curva de Nivel", sub=ecuacionParab, col.main="blue", scale=T,
      shade=0.5)
image(X,Y,z)
contour(X,Y,z,add=T)

###############################################################################################

x=seq(-2,2,length=50)
y=seq(-2,2,length=50)
parabola=function(x,y) x^2-y^2
z=outer(x, y, parabola)
ecuacionParab <- expression(z == x^2 - y^2)
persp(x,y,z,expand = 1,theta = 30, phi=30, col="green", main="Paraboloide", sub=ecuacionParab, col.main="blue", scale=T,
      border=NA, shade=0.5)
persp(x,y,z,expand = 1,theta = 30, phi=30, col="red", main="Paraboloide", sub=ecuacionParab, col.main="blue", scale=T,
      shade=0.3)
#############################  Curvas de Nivel ###################################
image(x,y,z)
contour(x,y,z,add=T)


####################################################################################


x=seq(-2,2,length=50)
y=seq(-2,2,length=50)
parabola=function(x,y) -sqrt(x^2-y^2)
z=outer(x, y, parabola)
ecuacionParab <- expression(z == x^2 - y^2)
persp(x,y,z,expand = 1,theta = 30, phi=30, col="green", main="Paraboloide", sub=ecuacionParab, col.main="blue", scale=T,
      border=NA, shade=0.5)
persp(x,y,z,expand = 1,theta = 90, phi=90, col="red", main="Paraboloide", sub=ecuacionParab, col.main="blue", scale=T,
      shade=0.3)
#############################  Curvas de Nivel ###################################
image(x,y,z)
contour(x,y,z,add=T)


####################################################################################




x=seq(-2,2,length=50)
a<-y=seq(-2,2,length=50)
parabola=function(x,y) -sqrt(x^2-y^2-a)
z=outer(x, y, parabola)
ecuacionParab <- expression(z == x^2 - y^2)
persp(x,y,z,expand = 1,theta = 30, phi=30, col="green", main="Paraboloide", sub=ecuacionParab, col.main="blue", scale=T,
      border=NA, shade=0.5)
persp(x,y,z,expand = 1,theta = 90, phi=90, col="red", main="Paraboloide", sub=ecuacionParab, col.main="blue", scale=T,
      shade=0.3)
#############################  Curvas de Nivel ###################################
image(x,y,z)
contour(x,y,z,add=T)


####################################################################################




x<-y<-seq(-2,2,length=50)
parabola=function(x,y) (x^2+3*y^2)*exp(1-x**2-y**2)
z=outer(x, y, parabola)
#ecuacionParab <- expression(z == x^2 - y^2)
persp(x,y,z,expand = 1,theta = 30, phi=30, col="green", main="Paraboloide", sub=ecuacionParab, col.main="blue", scale=T,
      border=NA, shade=0.5)
persp(x,y,z,expand = 1,theta = 30, phi=30, col="red", main="Paraboloide", sub=ecuacionParab, col.main="blue", scale=T,
      shade=0.3)
#############################  Curvas de Nivel ###################################
image(x,y,z)
contour(x,y,z,add=T)


####################################################################################




x<-seq(0,10,length=50)
y<-seq(0,10,length=50)
parabola=function(x,y) sin(x*y)
z=outer(x, y, parabola)
#ecuacionParab <- expression(z == x^2 - y^2)
persp(x,y,z,expand = 1,theta = 30, phi=30, col="green", main="Paraboloide", sub=ecuacionParab, col.main="blue", scale=T,
      border=NA, shade=0.5)
persp(x,y,z,expand = 1,theta = 45, phi=35, col="red", main="Paraboloide", sub=ecuacionParab, col.main="blue", scale=T,
      shade=0.3)
#############################  Curvas de Nivel ###################################
image(x,y,z)
contour(x,y,z,add=T)


####################################################################################



library(animation)

saveGIF({
  for(i in 1:100){
    x <- seq(0 + (i * 0.05), 3 + (i * 0.05), length= 100)
    y <- x
    f <- function(x, y) { sin(x*y) }
    z <- outer(x, y, f)
    persp(x, y, z, theta = 45, phi = 35, expand = 0.4, col = "green", border=NA, shade=0.5)
  }
}, interval = 0.1, ani.width = 550, ani.height = 550)


saveGIF({
  for(i in 1:150){
    x <- seq(-6 + (i * 0.05), 6 + (i * 0.05), length= 100)
    y <- x
    f <- function(x, y) { sin(x) + cos(y) }
    z <- outer(x, y, f)
    persp(x, y, z, theta = 45 + (i * 0.5), phi = 35, expand = 0.4, col = "lightblue")
  }
}, interval = 0.1, ani.width = 550, ani.height = 550)



Sys.Date()


 