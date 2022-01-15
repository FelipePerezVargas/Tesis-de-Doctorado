rm(list=ls())
library(ggplot2)
library(plotly)
library(ggthemes)
library(extrafont)
teq <- read.table("/Users/User/Desktop/MATLAB/recnuevos.txt", header=FALSE, sep="", na.strings="NA", dec=".", strip.white=TRUE)

plot(teq[,1],teq[,2],xaxs="i",yaxs="i",xlim=c(245,325),ylim=c(0.054,3),type='l',col="green",xlab="",ylab="",  lwd = 3, axes=F) 
for(i in 3:48){
  points(teq[,1],teq[,i],type='l',col=1:48,xlab="",ylab="",  lwd = 3)   
}
#axis(side = 1, at = seq(245, 325, by = 5), labels = FALSE, tcl = 0.2)
axis(1, pos=1)
axis(2, pos=0)




fur <- read.table("/Users/User/Desktop/MATLAB/nfur.txt", header=TRUE, sep="", na.strings="NA", dec=".", strip.white=TRUE)

plot(fur[,1],fur[,2],xaxs="i",yaxs="i",xlim=c(245,325),ylim=c(0.054,max(fur[,11])),type='l',col=rgb(0.5, .3, 0.8),xlab="",ylab="",  lwd = 3) 
for(i in 3:11){
points(fur[,1],fur[,i],type='l',col=rgb(0.5, .3, 0.8),xlab="",ylab="",  lwd = 3)   
}

names(fur)

 # for(i in 2:11){
f1<- ggplot(data=fur, aes(x=l.nm., y=c1)) +
  geom_line(
#}
labs(title="Plot of lengthby dose",x="Dose (mg)", y = "Length")
f1 + scale_color_manual(values=c('#999999','#E69F00'))


x <- seq(10,200,10)
y <- runif(x)
plot(x,y,xaxt='n',xaxs="i",yaxs="i" )
axis(side = 1, at = x,labels = T)

plot(c(0.5,2.5),c(2.5,4.5),xlim=c(0,3),ylim=c(2,5), axes=F)
  axis(1, pos=2)
  axis(2, pos=0)

str(mpg)


library(foreign)
library(gdata)
#df = read.csv("newteq.csv", header = FALSE);df
#df<-read.xls("newteq.xlsx")

#df <- read.xls("newteq", sheetIndex = 1)
#for(i  in 2: 11){
p<- qplot(l.nm., c1, data=fur, color=c4);p
#}
p1<- qplot(l.nm.,c1, data=fur, geom=c('point', 'smooth'));p1


p2<- qplot( c1, data=fur, fill=c2);p2

p3<- qplot( c1, data=fur, fill=c2);p3

#p4<- qplot(l.nm., c1, data=fur, facets=.~c3);p4

aa<-load("C:/Users/User/Downloads/maacs (1).Rda");aa
str(maacs)
qplot(log(eno), data=maacs, fill=mopos)

qplot(log(eno), data=maacs, geom='density')
qplot(log(eno), data=maacs, geom='density', color=mopos)

g<-ggplot(fur, aes( l.nm., c10));g
summary(g)
print(g)
h<-ggplot(fur, aes( l.nm., c9));h
print(h)
(p<-g+geom_line(aes(color=c10), size=2)+labs(title='Furfural')+labs(x='Longitud de onda(nm)', y='Absorbancia (UA)')
+ geom_smooth(size=1, linetype=3, method=lm) + ylim(0,2)+xlim(245,325))

(p1<-g+geom_line(aes(color=c10), size=1)
           +labs(title='Furfural')
           +labs(x='Longitud de onda(nm)', y='Absorbancia (UA)')
           #+ geom_smooth(size=1, linetype=3, method=lm)
           + coord_cartesian(ylim=c(0,2), xlim=c(245,325))
          )
 
 try<-ggplot(fur, aes(x=l.nm.,y= c1))
 try1<-ggplot(fur, aes(x=l.nm.,y= c2))
 try + geom_line()
     
 ggplot() + 
  geom_line(data = fur, aes(x=l.nm.,y= c1), color = "blue") +
  geom_line(data = fur, aes(x=l.nm.,y= c2), color = "blue") +
  geom_line(data = fur, aes(x=l.nm.,y= c3), color = "blue") +
  geom_line(data = fur, aes(x=l.nm.,y= c4), color = "blue") +
  geom_line(data = fur, aes(x=l.nm.,y= c5), color = "blue") +
  geom_line(data = fur, aes(x=l.nm.,y= c6), color = "blue") +
  geom_line(data = fur, aes(x=l.nm.,y= c7), color = "blue") +
  geom_line(data = fur, aes(x=l.nm.,y= c8), color = "blue") +
  geom_line(data = fur, aes(x=l.nm.,y= c9), color = "blue") +
  geom_line(data = fur, aes(x=l.nm.,y= c10), color = "blue") +
  xlab('Longitud de Onda (nm)') +
  ylab('Absorbancia (UA)')
  
    
  
   for(i in 1:10){
 p<-ggplot()  +  geom_line(data = fur, aes(x=l.nm.,y= fur[,i]), color = "blue") 
  } 
 p
  df <- data.frame(l.nm., c1, c2, c3,c4,c5,c6,c7,c8,c9,c10)
  
    my_grob = grobTree(textGrob("Cada día quedan más fresas", x=0.1,  y=0.95, hjust=0,
  gp=gpar(col="blue", fontsize=15, fontface="italic")))
  
  ggplot(fur, aes(x=l.nm., y = value, color = Concentración)) + 
    geom_point(aes(y = c1, col = "0.1 ppm")) + 
    geom_point(aes(y = c2, col = "0.2 ppm")) +
    geom_point(aes(y = c3, col = "0.3 ppm")) +
    geom_point(aes(y = c4, col = "0.4 ppm")) +
    geom_point(aes(y = c5, col = "0.5 ppm")) +
    geom_point(aes(y = c6, col = "0.6 ppm")) +
    geom_point(aes(y = c7, col = "0.7 ppm")) +
    geom_point(aes(y = c8, col = "0.8 ppm")) +
    geom_point(aes(y = c9, col = "0.9 ppm")) +
    geom_point(aes(y = c10, col = "1.0 ppm "))+
    xlab('Longitud de Onda (nm)') +
    ylab('Absorbancia (UA)') +
    ggtitle('Curva de Calibración Furfural')+
    scale_x_continuous(limits=c(245,325)) +
    theme(panel.background = element_rect(fill = 'grey75'),
    panel.grid.major = element_line(colour = "orange"),
    panel.grid.minor = element_line(colour = "blue"))+
    theme(plot.background = element_rect(fill = 'pink'))+
   # annotation_custom(my_grob)+
    theme( axis.text.x=element_text(angle=90, size=8),
    axis.title.x=element_text(angle=10, color='red'),
    axis.title.y=element_text(angle=80, color='blue', face='bold', size=14))
    
    
     ggplot(fur, aes(x=l.nm., y = value, color = Concentración)) + 
    geom_line(aes(y = c1, col = "0.1 ppm")) +  
    geom_line(aes(y = c2, col = "0.2 ppm")) +
    geom_line(aes(y = c3, col = "0.3 ppm")) +
    geom_line(aes(y = c4, col = "0.4 ppm")) +
    geom_line(aes(y = c5, col = "0.5 ppm")) +
    geom_line(aes(y = c6, col = "0.6 ppm")) +
    geom_line(aes(y = c7, col = "0.7 ppm")) +
    geom_line(aes(y = c8, col = "0.8 ppm")) +
    geom_line(aes(y = c9, col = "0.9 ppm")) +
    geom_line(aes(y = c10, col = "1.0 ppm "))
              
  
    
    
    FsuperfRect=function(){
  cat("Longitud de la base: ");   
    base=scan(n=1,quiet=TRUE) 
    cat("Longitud de la altura: "); 
    altura=scan(n=1,quiet=TRUE) 
    S=base*altura 
    cat("Superficie del rectángulo: ",S) 
  }
 FsuperfRect()
  
 str(fur)
 pq<-qplot(l.nm., c1, data=fur)
 pq
 str(teq)