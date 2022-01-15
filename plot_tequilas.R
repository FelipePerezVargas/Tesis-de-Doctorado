rm(list=ls())
library(ggplot2)
library(plotly)
library(ggthemes)
library(extrafont)
library(plyr)
library(gridExtra)
library(dplyr)
library(TurtleGraphics)
library(animation)
library(hrbrthemes)
library(tidyr)
library(viridis)
library(tidyverse)
library(purrr)

##################################################################################################
######################################## Tequilas ###############################################
#################################################################################################
tequilas <- read.table("/Users/Admin/Downloads/Respaldo_recuperación_tesis/final_teq.txt", 
                       header=TRUE, sep="", na.strings="NA", dec=".", strip.white=TRUE)


 



saveGIF({
  for(lim in seq(-3.14,3.14,by=0.1))
    curve(sin, from=lim,  to=lim + 9)
}, movie.name="animacion.gif", interval=0.2, ani.width=640, ani.height=640)




for(i in 2:48){
  pt<-ggplot(tequilas, aes(x=nm, y =tequilas[,i], color = V1)) + 
    geom_line(aes(y = tequilas[,i], col =paste('jhgjhg')))
  pt[i]<-ggplotly(pt);pt
}
#dev.off()


######################## Tequilas por separado ####################

par(mfrow=c(4,2))
for(i in 2:48){
  plot(tequilas[,1], tequilas[,i], type='l', xlab='Longitud de Onda (nm)', ylab = 'Espectro de Absorción Molar (ua)',
       col.lab='black',panel.first = grid( col = "gray", lty = "solid", lwd = 0.5),main = 'fghfdg')
}

library(RColorBrewer)
###############################  Tequilas juntos ##################
plot(tequilas[,1], tequilas[,2], type='l', lwd=2.5, xlab="Longitud de Onda (nm)",ylab="Absorción (ua)", ylim =c(0,3))
for(i in 3:length(tequilas)){
  lines(tequilas[,1], tequilas[,i], type='l', lwd=2.5, col= c(1:length(tequilas)))  
}

for(i in 2:48){
a<- names(tequilas[,i])
}

names(tequilas[,2])

######################### Por separado en ggplot 2 pero falta mejorar #######################
par(mfrow=c(2,2))
for (i in 3:length(tequilas)) { 
  print(ggplot(tequilas,aes(tequilas[,1],tequilas[,2]))+geom_line(y=tequilas[,i])) +
  ggtitle('Espectro de Abosrción de Furfural')+
    theme( axis.line = element_line(colour = "darkblue",size = 2, linetype = "solid"))+
    scale_x_discrete(name ="Longitud de Onda (nm)", limits=seq(245,325,10))+
    scale_y_discrete(name="Absorbancia (UA)", limits=seq(0,5, 0.5))
}


d <- tequilas
d %>% str()
d %>% keep(is.numeric) %>% head()
for (col in d) {
  # Plot col
}
d %>%
  keep(is.numeric) %>% 
  gather() %>%
  head()
d %>%
  keep(is.numeric) %>%                     # Keep only numeric columns
  gather() %>%                             # Convert to key-value pairs
  ggplot(aes(value)) +                     # Plot the values
  facet_wrap(~ key, scales = "free") +   # In separate panels
  geom_density()+labs(title = "Espectro de Absorción Muestra de Tequila", x = "Longitud de onda (nm)", 
 y = "Aborsorbancia (ua)")
 +scale_x_discrete(name ="Longitud de Onda (nm)", limits=seq(245,325,10))+
  scale_y_continuous(name="Absorbancia (UA)", limits=c(0, 2))

  
######### Todos juntos ########  
  head(tequilas)
df_tequilas <- gather(tequilas, m2, m1, -nm)
qplot(data=df_tequilas, x=nm, y=m1)







ggplot(df_tequilas, aes(x=nm, y=m1)) +
  geom_line(aes(color=m2), size=0.8)+
  ylab("Longitud de Onsa (nm)")+
  xlab("Absorbancia (ua)") +
  theme_light(base_size = 2) +
ggtitle("Espectros de Absorción Tequilas")




ggplot(tequilas) +
  geom_density(aes(x=nm, group=m1), alpha=0.6, adjust=0.75) + 
  geom_rect(data=tequilas[2,], aes(xmin=245, xmax=325, ymin=0,ymax=2), fill="red", alpha=0.2)






library(gridExtra)
library(ggplot2)

for(i in 2:48){
qplot(tequilas[,1], tequilas[,i], data = tequilas, geom = c("point", "smooth"))
}



######################## Tequilas por separado ####################
b<-vector()
for(i in 2:48){
  a<-max(tequilas[,i])
  a<-as.numeric(a)
  print(a)
  b<-a[i]
}


maxt<-c(3.8244,4.0609,5.8269,3.366,3.5643,3.98,3.9948,6,4.2326,3.9112,3.6256,3.4194,3.5643,3.4446,6,3.9497, 3.6508,
        3.1209,3.0528,4.8446,6,3.2146,6,3.7719,3.6427,6,6,2.9848,3.0197,3.1021,3.1444,3.7352,4.0344,3.7799,3.7779,
        3.7821,3.7943,3.835,3.0326,3.5385,3.8159,3.187,2.9877,3.1203,3.2202,3.111,3.8468)
m<-matrix(maxt,47,1)

par(mfrow=c(4,2))
for(i in 2:48){
  plot(tequilas[,1], tequilas[,i], type='l', xlab='Longitud de Onda (nm)', ylab = 'Espectro de Absorción Molar (ua)',
       col.lab='black',panel.first = grid( col = "gray", lty = "solid", lwd = 0.5),
       main=paste("Espectro de Absorción de muestra", i-1), xlim =c(245,325), ylim =c(0,max(tequilas[,i])))
}


n <- c(1:10)
# Create empty list to store vectors
list_of_vecs <- list()

# Create n vectors of random numbers - length 10. This works ok.
for (i in n){
  list_of_vecs[[i]]<-rnorm(10,0,1)
}

#If you really want to use "assign":
  for (i in n){
    vecname<-paste('vec_', i, sep = '')
    assign(vecname, rnorm(10,0,1))
    list_of_vecs[[i]]<-get(vecname)
  }


b<-vector()
for(i in 2:48){
m<-c(tequilas[i,])
m<-as.numeric(m)
#a<-max(m)
}

res <- numeric(length = 47)
for (i in 2:48) {
  res[i] <- max(tequilas[,i])
}
res


m<-matrix(res,48,1)
res1<-seq(1:48)
plot(res1,res)



