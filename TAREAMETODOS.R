rm(list=ls())

##CAMBIAR GRADOS FARENHEIT KELVIN##
ftok<-function(gF){
  gK<-((gF-32)*(5/9))+273.15
  cat("Los grados kelvin son:")
  return(gK)
}
ftok(5)

ftof<-function(gK){
  gF<-((gK+255.92))
  cat("Los grados Farenheit son:")
  return(gF)
}
ftof(5)

##CONVERTIR KILOGRAMOS A DIFERENTES UNIDADES DE MASA##
masa<-function(kg){
  gramos<-kg*1000
  toneladas<-kg/1000
  miligramos<-kg/0.000001
  libra<-kg*0.45 
  hectogramo<-kg*0.1
  print(paste("La masa es:", gramos , "gramos"))
  print(paste("La masa es:", toneladas , "toneladas"))
  print(paste("La masa es:", miligramos , "miligramos"))
  print(paste("La masa es:", libra , "libra"))
  print(paste("La masa es:", hectogramo , "hectogramo"))
  
  return(gramos)
}
masa(5)


##CONVERTIR MINUTOS A DIFERENTES UNIDADES DE TIEMPO##
tiempos<-function(min) {
  segundos<-min/60
  hora<-min*3600
  microsegundos<-min/0.000001
  a??o<-min*31540000 
  mes<-min*2628000
  print(paste("El tiempo es:", segundos , "segundos"))
  print(paste("El tiempo es:", hora , "horas"))
  print(paste("El tiempo es:", microsegundos , "microsegundos"))
  print(paste("El tiempo es:", a??o , "a??os"))
  print(paste("El tiempo es:", mes, "meses"))
  
  return(tiempos)
}
tiempos(5)
