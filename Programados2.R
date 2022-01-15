hipotenusa <- function(x, y) {
sqrt(x^2 + y^2)
}
hipotenusa(3,4)


PI <- 3.1416
ff <- function(r) {
return(PI * r^2)
}
ff(4)

fuerza <- function(m,a){
       m*a
 }
 fuerza(2,3)
  
 Energia <- function(m,v){
 0.5*m*v^2
 }
 Energia(10,10)
 
 
 letras <- c("c", "l", "i", "M", "T", "A")
for (i in 1: length(letras)) {
print(letras[i])
}
 for (i in seq_along(letras)) {
print(letras[i])
}

for (letra in letras) {
print(letra)
}

i <- 1
while (i <= 6) {
print(letras[i])
i <- i + 1
}

i <- 1
repeat {
print(letras[i])
i <- i + 1
if (i > 6)
break
}

MiFunc.v1 <- function (x, yyy, z=5, t) {
w <- x + yyy + z
w
}
MiFunc.v1


# se usará un generador de números aleatorios,
# la siguiente función asegura su repetibilidad:
set.seed(140) # el argumento puede ser cualquier número
aprox <- 0.003 # Valor determinante para la salida del ciclo
Y_ini <- 2.7 # Supuesto valor inicial de Y
for (iter in 1: 1000) { # aseguro no más de 1000 iteraciones
# Procedimiento para calcular la siguiente Y, que
# en este caso simularemos mediante generador aleatorio:
Y <- Y_ini + 0.008*rnorm(1)
# La condición de salida:
if (abs(Y - Y_ini) <= aprox)
break # Uso del break para salir del ciclo
# Preparamos para la siguiente iteración
Y_ini <- Y
}
# Veamos que ha resultado:
paste0("Y_ini: ", Y_ini, ", Y: ", Y, ", Num.iter: ", iter)

fun <- function(){
  x <- readline("What is the value of x?")  
  y <- readline("What is the value of y?")
  t <- readline("What are the T values?")
  v <- readline("What are the V values?")

  x <- as.numeric(unlist(strsplit(x, ",")))
  y <- as.numeric(unlist(strsplit(y, ",")))
  t <- as.numeric(unlist(strsplit(t, ",")))
  v <- as.numeric(unlist(strsplit(v, ",")))

  out1 <- x + y
  out2 <- t + v

  return(list(out1, out2))
}



silly <- function()
{
   age <- ask("How old aroe you? ")
   age <- as.numeric(age)
   cat("In 10 years you will be", age+10, "years old!\n")
}
