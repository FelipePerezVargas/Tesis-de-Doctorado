parameters <- c(s = 10, r = 28, b = 8/3)
state <- c(X = 0, Y = 1, Z = 1)
 
Lorenz <- function(t, state, parameters) {
    with(as.list(c(state, parameters)), {
        dX <- s * (Y - X)
        dY <- X * (r - Z) - Y
        dZ <- X * Y - b * Z
        list(c(dX, dY, dZ))
    })
}

times <- seq(0, 100, by = 0.01)
library(deSolve)
out <- ode(y = state, times = times, func = Lorenz, parms = parameters)
 
par(oma = c(0, 0, 3, 0))
plot(out, xlab = "time", ylab = "-")
plot(out[, "Y"], out[, "Z"], pch = ".", type = "l")
mtext(outer = TRUE, side = 3, "Lorenz model", cex = 1.5)


# create factors with value labels 
fur$c1 <- factor(fur$c1,levels=c(3,4,5),
  	labels=c("3gears","4gears","5gears")) 
fur$c2 <- factor(fur$c2,levels=c(0,1),
  	labels=c("Automatic","Manual")) 
fur$c3 <- factor(fur$c3,levels=c(4,6,8),
   labels=c("4cyl","6cyl","8cyl")) 
qplot(l.nm., data=fur, geom="density", fill=c2, alpha=I(.5), 
   main="Distribution of Gas Milage", xlab="Miles Per Gallon", 
   ylab="Density")