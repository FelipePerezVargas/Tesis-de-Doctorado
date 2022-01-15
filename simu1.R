
saveGIF({
  
  ani.options(interval = 0.2, nmax = 50)
  
  t = seq(0,pi,.01)
  
  x = cos(2*t)
  
  y = sin(2*t)
  
  idx = seq(1,length(x),10)
  
  for (i in seq_along(idx)) {
    
    plot(x,y,type='n')
    
    points(x[seq(idx[i])],
           
           y[seq(idx[i])], pch=15, col='dark green')
    
    ani.pause() }
  
}, movie.name = "circle.gif",

ani.width = 600, ani.height = 600)

saveGIF({
  for (i in 1:10) plot(runif(10), ylim = 0:1)
})

saveGIF({
  for(i in 1:100){
    curve(sin(x), from =  (i * 0.05), to = 5 + (i * 0.05), col = "red", ylab = "")
    curve(cos(x), from =  (i * 0.05), to = 5 + (i * 0.05), add = TRUE, col = "blue", ylab = "")
    legend("topright", legend = c("sin(x)", "cos(x)"), fill = c("red", "blue"), bty = "n")
  }
}, interval = 0.1, ani.width = 550, ani.height = 350)




bisection.method(FUN = function(x) x^2 - 4, rg = c(-1, 10), tol = 0.001,
                 interact = FALSE, main='sfds', xlab='fsdf', ylab='fdsfs')
