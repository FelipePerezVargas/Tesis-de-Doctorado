################ Conversiones de longitud ~ In a cm~ ###############
pulgada<-function(In)
{
  centimetros<-(In*2.54)
  print(paste('centimetros:', centimetros,'cm'))
  return(centimetros)
}
pulgada(6)
#########cm a In#######################
cm<-function(cm)
{
  pulgadas<-(cm/2.54)
  print(paste('pulgadas:', pulgadas,'In'))
  return(pulgadas)
}
cm(6)
#########conversion de cm a pies###########
pies<-function(Ft)
{
  centimetros<-(Ft*30.48)
  print(paste('centimetros:', centimetros,'cm'))
  return(centimetros)
}
pies(6)
#######conversion de pies a cm################
cm<-function(cm)
{
  pies<-(cm/30.48)
  print(paste('pies:', pies,'ft'))
  return(pies)
}
cm(6)
