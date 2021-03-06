rm(list=ls())
library(ggplot2)
library(plotly)
library(ggthemes)
library(extrafont)
library(grid)
library(gridExtra)
library(gapminder)


hplc <- read.table("/Users/User/Desktop/MATLAB/calhplc.txt", header=TRUE, sep="", na.strings="NA", dec=".", strip.white=TRUE)




fit <- lm(va ~ co, data = hplc)
blue.bold.italic.16.text <- element_text(face = "bold.italic", color = "blue", size = 16)
red.bold.italic.text <- element_text(face = "bold.italic", color = "blue")
equation = function(x) {
  lm_coef <- list(a = round(coef(x)[1], digits = 2),
                  b = round(coef(x)[2], digits = 2),
                  r2 = round(summary(x)$r.squared, digits = 4));
  lm_eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(R)^2~"="~r2,lm_coef)
  as.character(as.expression(lm_eq));                 
}

#pdf(file = "hmfhplc.pdf", width = 8, height = 10)
va <- ggplot(hplc, aes(x=co, y=va)) + geom_point(color="red",fill="blue",shape=21,alpha=0.5,size=4,stroke = 2) +
  geom_smooth(method=lm, col="springgreen4") +
  ggtitle("Curva de calibraci�n Vainillina ", subtitle="Cromatograf�a de L�quidos de Alta Resoluci�n HPLC") +
  scale_x_continuous(name = "Concentraci�n (ppm)") +
  scale_y_continuous(name = "�rea (mUA)") +
  coord_cartesian(xlim=c(0,10), ylim=c(0, max(hplc[,6])))+
  #Inicia elementos de tema
  theme(panel.background = element_rect(fill = 'gray94'),
        panel.grid.major = element_line(colour = "azure", size=1.5),
        panel.grid.minor = element_line(colour = "tomato", size=.25,linetype = "dashed"),
        panel.border = element_blank(),
        axis.line.x = element_line(colour = "blue", size=1.5,lineend = "butt"),
        axis.line.y = element_line(colour = "blue", size=1.5), 
        axis.text.x = element_text(face="bold", color="red", size=14, angle=00),
        axis.text.y = element_text(face="bold", color="red", size=14, angle=0),
        title = red.bold.italic.text, axis.title = blue.bold.italic.16.text) +
  #Fin del tema
  annotate("rect", xmin = -0.3, xmax = 3.7, ymin = 105000, ymax = 121000, fill="white", colour="black") +
  annotate("text", x = 1.7, y = 115000, label = equation(fit), parse = TRUE)
va
#dev.off()


