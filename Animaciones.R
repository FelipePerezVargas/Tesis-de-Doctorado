rm(list=ls())

library(animation)

saveGIF({
  for(i in 1:50){
    curve(sin(x)**2+cos(x)**2, from = -10 + (i * 0.01), to = 10 + (i * 0.01), col = "red", ylab = "", lwd='2')
    #curve(cos(x), from = -10 + (i * 0.01), to = 10 + (i * 0.01), add = TRUE, col = "blue", ylab = "", lwd='2')
    #curve(0*x, from = -10 + (i * 0.01), to = 10  + (i * 0.01), add = TRUE, col = "green", ylab = "", lwd='2')
    legend("topright", legend = c("sin(x)", "cos(x)", '0'), fill = c("red", "blue", 'green'), bty = "n")
  }
}, interval = 0.01, ani.width = 550, ani.height = 350)



saveGIF({
  theta<-30
  Vo<-20
  g<-9.8
    for(i in 1:50){
    curve( x*tan(theta*(180/pi))-((g*x**2)/(2*Vo**2*cos(theta*(180/pi))**2)), from = 0 + (i * 0.1), to = 25+(i * 0.1) , 
          col = "red",  ylab = "", lwd='2')
      legend("topright", legend = "Tiro Parábolico", bty = "n")
  }
}, interval = 0.1, ani.width = 550, ani.height = 350)


library(rgl)
open3d()            
spheres3d(x = 1, y = 1, z = 1, radius = 1)
axes3d()   


x <- seq(-1, 1, length=21)
       Z <- outer(x, x, function(x, y)sqrt(1-x**2-y**2))
       persp(x=x, y=x, z=Z)


image(x=x, y=x, z=Z)
contour(x=x, y=x, z=Z, add=T)


print(car2sph(x=1,y=1,z=0,deg=TRUE))
pointsphere(N = 100, longlim = c(0, 360), latlim = c(-90, 90), rlim = c(0, 1))
rgl.sphgrid()
rgl.sphMW()

rgl.sphgrid()
rgl.sphpoints(40,50,0.5,deg=TRUE,col='red',cex=2)
rgl.sphpoints(-40,50,0.8,deg=TRUE,col='green',cex=2)


rgl.sphgrid()
rgl.sphtext(40,50,0.5,'HI!',deg=TRUE,col='red',cex=2)


print(sph2car(45,0,sqrt(2),deg=TRUE))



rgl.sphgrid()
rgl.sphsun()
rgl.sphgrid()
rgl.sphsun('get',radius=2,col='red')

open3d()
spheres3d(rnorm(100), rnorm(100), rnorm(100), radius = runif(100), color = rainbow(100))



rgl.sphsun(Ydate = c(3, 21), radius = 1, col = "red", type = "s", sunrad = 0.02,
           addeclip = TRUE, addsun=TRUE)

open3d()
cols <- rainbow(7)
layout3d(matrix(1:16, 4,4), heights=c(1,3,1,3))
text3d(0,0,0,"tetrahedron3d"); next3d()
shade3d(tetrahedron3d(col=cols[1])); next3d()
text3d(0,0,0,"cube3d"); next3d()
shade3d(cube3d(col=cols[2])); next3d()
text3d(0,0,0,"octahedron3d"); next3d()
shade3d(octahedron3d(col=cols[3])); next3d()
text3d(0,0,0,"dodecahedron3d"); next3d()
shade3d(dodecahedron3d(col=cols[4])); next3d()
text3d(0,0,0,"icosahedron3d"); next3d()
shade3d(icosahedron3d(col=cols[5])); next3d()
text3d(0,0,0,"cuboctahedron3d"); next3d()
shade3d(cuboctahedron3d(col=cols[6])); next3d()
text3d(0,0,0,"oh3d"); next3d()
shade3d(oh3d(col=cols[7]))

f <- function(x,y) x^2 - y^2 
x <- y <- seq(-10,10,length=100)
z <- outer(x,y,f)
contour(x=x, y=x, z=z, levels=0, las=1, drawlabels=FALSE, lwd=3)


x <- y <- seq(-4*pi, 4*pi, len = 27)
r <- sqrt(outer(x^2, y^2, "+"))
filled.contour(cos(r^2)*exp(-r/(2*pi)), axes = FALSE)
## rather, the key *should* be labeled:
filled.contour(cos(r^2)*exp(-r/(2*pi)), frame.plot = FALSE,
               plot.axes = {})

a <- expand.grid(1:20, 1:20)
b <- matrix(a[,1] + a[,2], 20)
filled.contour(x = 1:20, y = 1:20, z = b,
               plot.axes = { axis(1); axis(2); points(10, 10) })

ellipse3d(x, scale = c(1,1,1), centre = c(0,0,0), 
          level = 0.95)
