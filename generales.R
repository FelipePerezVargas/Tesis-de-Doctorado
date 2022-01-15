persona <- c("Hugo", "Paco", "Luis", "Petra", "Maria", "Fulano",
"Sutano", "Perengano", "Metano", "Etano", "Propano")
mes.nacimiento <- c("Dic", "Feb", "Oct", "Mar", "Feb", "Nov",
"Abr", "Dic", "Feb", "Oct", "Dic")
Fmes.nacimiento <- as.factor(mes.nacimiento)
Fmes.nacimiento
unclass(Fmes.nacimiento)
table(Fmes.nacimiento)
meses <- c("Ene", "Feb", "Mar", "Abr", "May", "Jun", "Jul", "Ago",
"Sep", "Oct", "Nov", "Dic")
FFmes.nacimiento <- factor(mes.nacimiento, levels=meses)
FFmes.nacimiento
table(FFmes.nacimiento)
familia <- list("Maria", "Juan", 10, c("Hugo", "Petra"), c(8,6))
familia
familia <- list(madre="Maria", padre="Juan", casados=10,
hijos=c("Hugo", "Petra"), edades=c(8, 6))

(m <- cbind(ord=1: 3, edad=c(30L, 26L, 9L)) )
(v <- c(1.80, 1.72, 1.05) )
(ff <- data.frame(familia=c("Padre", "Madre", "Hijo"),m, estatura=v))

write(v, "OtroArchivo.txt")
(v1 <- scan("OtroArchivo.txt"))

frutas <- c(15, 100, 2, 30)
names(frutas) <- c("naranja", "pera", "manzana", "durazno")
(frutas <- c(naranja = 15, pera = 100, manzana = 2, durazno = 30))


g <- 9.81 # aceleración gravedad
x0 <- 0 # x inicial
y0 <- 15 # y inicial
vi <- 7 # velocidad inicial
alphaD <- 50 # ángulo-grados
# Se convierte a radianes
alpha <- (pi/180) * alphaD # angulo-radianes
(vox <- vi * cos(alpha)) # componente x de velocidad inicial
(voy <- vi * sin(alpha)) # componente y de velocidad inicial
las.x <- seq(from = 0, to = 11, by = 0.5)
las.t <- (las.x - x0) /vox
(las.y <- -(g/2) * las.t^2 + voy * las.t + y0)
plot(las.x,las.y)
abline(h = 0, v = 0, col = "gray60")

## Setup up coordinate system (with x == y aspect ratio):
plot(c(-2,3), c(-1,5), type = "n", xlab = "x", ylab = "y", asp = 1)
## the x- and y-axis, and an integer grid
abline(h = 0, v = 0, col = "gray60")
text(1,0, "abline( h = 0 )", col = "gray60", adj = c(0, -.1))
abline(h = -1:5, v = -2:3, col = "lightgray", lty = 3)
abline(a = 1, b = 2, col = 2)
text(1,3, "abline( 1, 2 )", col = 2, adj = c(-.1, -.1))

## Simple Regression Lines:
require(stats)
sale5 <- c(6, 4, 9, 7, 6, 12, 8, 10, 9, 13)
plot(sale5)
abline(lsfit(1:10, sale5))
abline(lsfit(1:10, sale5, intercept = FALSE), col = 4) # less fitting

z <- lm(dist ~ speed, data = cars)
plot(cars)
abline(z) # equivalent to abline(reg = z) or
abline(coef = coef(z))

## trivial intercept model
abline(mC <- lm(dist ~ 1, data = cars)) ## the same as
abline(a = coef(mC), b = 0, col = "blue")
#Let's add a blue line with intercept 2 and slope 2 to the plot:
>abline(a=2,b=2,col="blue")

(m <- matrix(11: 30, nrow = 5, ncol = 4, byrow = TRUE))
rownames(m) <- c("uno", "dos", "tres", "cuatro", "cinco")
colnames(m) <- c("UNO", "DOS", "TRES", "CUATRO");m


(familia <- list(madre="Maria", padre="Juan", casados=10,hijos=c("Hugo", "Petra"), edades=c(8, 6)))

(mt <- matrix(11: 30, nrow = 4, ncol = 5))
(df.mt <- as.data.frame(mt))
mt[, 5] == 16
df.mt[, 2] == 16

mt[(mt[, 2] == 16), ]
df.mt[(df.mt[, 2] == 16), ]
mt[2, ] %%8 != 0 
mt[, (mt[2, ] %%8 != 0)]


