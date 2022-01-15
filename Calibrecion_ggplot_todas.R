rm(list=ls())
library(ggplot2)
library(plotly)
library(ggthemes)
library(extrafont)
library(plyr)


con<-seq(0.1:1.0)
vai<-vector()
#pdf(file = "vainillina.pdf", width = 8, height = 10)
vai <- read.table("/Users/User/Desktop/porfavor/nvai.txt", header=TRUE, sep="", na.strings="NA", dec=".", strip.white=TRUE)
p7<-ggplot(vai, aes(x=nm, y = value, color = Concentración)) + 
  geom_line(aes(y = c1, col = "1 ppm")) + 
  geom_line(aes(y = c2, col = "2 ppm")) +
  geom_line(aes(y = c3, col = "3 ppm")) +
  geom_line(aes(y = c4, col = "4 ppm")) +
  geom_line(aes(y = c5, col = "5 ppm")) +
  geom_line(aes(y = c6, col = "6 ppm")) +
  geom_line(aes(y = c7, col = "7 ppm")) +
  geom_line(aes(y = c8, col = "8 ppm")) +
  geom_line(aes(y = c9, col = "9 ppm")) +
  geom_line(aes(y = c10, col = "10 ppm "))+
  ggtitle('Espectro de Abosrción del Siringaldehido')+
  theme( axis.line = element_line(colour = "darkblue",size = 2, linetype = "solid"))+
  scale_x_discrete(name ="Longitud de Onda (nm)", limits=seq(245,325,10))+
  scale_y_continuous(name="Absorbancia (UA)", limits=c(0, 0.75))
p7 <- ggplotly(p7);p7
#dev.off()
