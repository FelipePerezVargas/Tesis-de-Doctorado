rm(list=ls())
library(ggplot2)
library(plotly)
library(ggthemes)
library(extrafont)
library(grid)
library(gridExtra)
library(gapminder)


hplc <- read.table("/Users/User/Desktop/MATLAB/calhplc.txt", header=TRUE, sep="", na.strings="NA", dec=".", strip.white=TRUE)


#qplot(co, hmf5, data=hplc, data=, color=, shape=, size=, alpha=, geom=, method=, formula=, facets=, xlim=, ylim= xlab=, ylab=, main=, sub=)
par(mfrow = c(3, 2))
gg<- ggplot() + 
  geom_point(data =hplc, aes(y=hmf5,x= co), color = "blue") +
  geom_point(data =hplc, aes(y=fu2,x= co), color = "#999999") +
  geom_point(data =hplc, aes(y=a2,x= co), color = "#56B4E9") +
  geom_point(data =hplc, aes(y=m5,x= co), color = "#009E73") +
  geom_point(data =hplc, aes(y=va,x= co), color = "#F0E442") +
  geom_point(data =hplc, aes(y=si,x= co), color = "#0072B2") +
  coord_cartesian(xlim=c(0,10), ylim=c(0, max(hplc[,2])+21000))+
  ggtitle("Curva de calibración de 2Acetil-Furan", subtitle="Cromatografía de Líquidos de Alta Resolución") +
  xlab('Concentración (ppm)') +
  ylab('Área  (mUA)')+
  theme(panel.background = element_rect(fill = 'grey'),
      panel.grid.major = element_line(colour = "burlywood", size=1.5),
      panel.grid.minor = element_line(colour = "tomato",size=.25,linetype = "dashed"),
      panel.border = element_blank(),axis.line.x = element_line(colour = "darkorange",size=1.5,lineend = "butt"),
      axis.line.y = element_line(colour = "darkorange",size=1.5))  
plot(gg)
  
qplot(x = co,y = hmf5,data = hplc)

#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%##%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#
#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%# 5 HIDROXIMETIL FURFURAL %#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#
#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%##%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#
  
a<- ggplot(data =hplc, aes(y=hmf5,x= co)) 
aa<-a+geom_point(col="green", size=3)+  geom_smooth(method="lm", col="firebrick")+
  ggtitle("Curva de calibración de 5 Hidroximetil Furfural", subtitle="Cromatografía de Líquidos de Alta Resolución") +
  xlab("Concentración (ppm)") + ylab("Área (mAU)")+
  coord_cartesian(xlim=c(0,10), ylim=c(0, max(hplc[,2])+21000))+
  theme(panel.background = element_rect(fill = 'grey'),
        panel.grid.major = element_line(colour = "burlywood", size=1.5),
        panel.grid.minor = element_line(colour = "tomato", 
                                        size=.25, 
                                        linetype = "dashed"),
        panel.border = element_blank(),
        axis.line.x = element_line(colour = "darkorange", 
                                   size=1.5, 
                                   lineend = "butt"),
        axis.line.y = element_line(colour = "darkorange", 
                                   size=1.5))  

#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%##%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#
#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#% FURFURAL %#%#%#%%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%
#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%##%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#
  
  b<- ggplot(data =hplc, aes(y=fu2,x= co)) 
 bb<- b+geom_point(col="green", size=3)+  geom_smooth(method="lm", col="firebrick")+
  ggtitle("Curva de calibración de 2Acetil-Furan", subtitle="Cromatografía de Líquidos de Alta Resolución") +
  xlab("Concentración (ppm)") + ylab("Área (mAU)")+
  coord_cartesian(xlim=c(0,10), ylim=c(0, max(hplc[,3])))+
    theme(panel.background = element_rect(fill = 'grey'),
          panel.grid.major = element_line(colour = "burlywood", size=1.5),
          panel.grid.minor = element_line(colour = "tomato", 
                                          size=.25, 
                                          linetype = "dashed"),
          panel.border = element_blank(),
          axis.line.x = element_line(colour = "darkorange", 
                                     size=1.5, 
                                     lineend = "butt"),
          axis.line.y = element_line(colour = "darkorange", 
                                     size=1.5))  
   
  
  #%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%##%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#
  #%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%# 2 ACETIL FURAN #%#%#%#%#%%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%
  #%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%##%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#
  
  c<- ggplot(data =hplc, aes(y=a2,x= co)) 
 cc<- c+geom_point(col="green", size=3)+  geom_smooth(method="lm", col="firebrick")+
    ggtitle("Curva de calibración de 2 Metil Furfural ", subtitle="Cromatografía de Líquidos de Alta Resolución") +
    xlab("Concentración (ppm)") + ylab("Área (mAU)")+
    coord_cartesian(xlim=c(0,10), ylim=c(0, max(hplc[,4])))+
    theme(panel.background = element_rect(fill = 'grey'),
          panel.grid.major = element_line(colour = "burlywood", size=1.5),
          panel.grid.minor = element_line(colour = "tomato", 
                                          size=.25, 
                                          linetype = "dashed"),
          panel.border = element_blank(),
          axis.line.x = element_line(colour = "darkorange", 
                                     size=1.5, 
                                     lineend = "butt"),
          axis.line.y = element_line(colour = "darkorange", 
                                     size=1.5))  
  
  
  
  #%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%##%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#
  #%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%# 5 METIL FURAN #%%#%#%#%#%%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%
  #%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%##%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#
  
  d<- ggplot(data =hplc, aes(y=m5,x= co)) 
 dd<- d+geom_point(col="green", size=3)+  geom_smooth(method="lm", col="firebrick")+
    ggtitle("Curva de calibración de 5 Metil Furan ", subtitle="Cromatografía de Líquidos de Alta Resolución") +
    xlab("Concentración (ppm)") + ylab("Área (mAU)")+
    coord_cartesian(xlim=c(0,10), ylim=c(0, max(hplc[,5])))+
    theme(panel.background = element_rect(fill = 'grey'),
          panel.grid.major = element_line(colour = "burlywood", size=1.5),
          panel.grid.minor = element_line(colour = "tomato", 
                                          size=.25, 
                                          linetype = "dashed"),
          panel.border = element_blank(),
          axis.line.x = element_line(colour = "darkorange", 
                                     size=1.5, 
                                     lineend = "butt"),
          axis.line.y = element_line(colour = "darkorange", 
                                     size=1.5))  
  
  
  
  
  #%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%##%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#
  #%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%# VAINILLINA  #%%#%#%#%%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%
  #%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%##%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#
  
  e<- ggplot(data =hplc, aes(y=va,x= co)) 
ee<- e+geom_point(col="green", size=3)+  geom_smooth(method="lm", col="firebrick")+
    ggtitle("Curva de calibración de Vainillina ", subtitle="Cromatografía de Líquidos de Alta Resolución") +
    xlab("Concentración (ppm)") + ylab("Área (mAU)")+
    coord_cartesian(xlim=c(0,10), ylim=c(0, max(hplc[,6])))+
    theme(panel.background = element_rect(fill = 'grey'),
          panel.grid.major = element_line(colour = "burlywood", size=1.5),
          panel.grid.minor = element_line(colour = "tomato", 
                                          size=.25, 
                                          linetype = "dashed"),
          panel.border = element_blank(),
          axis.line.x = element_line(colour = "darkorange", 
                                     size=1.5, 
                                     lineend = "butt"),
          axis.line.y = element_line(colour = "darkorange", 
                                     size=1.5))  
+
annotate("rect", xmin = 0.00, xmax = 5.1, ymin = 10000, ymax = 11000, fill="white", colour="red")
  plot(ee)
  #%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%##%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#
  #%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%# SIRINGALDEHÍDO #%%#%#%#%#%%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#
  #%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%##%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#
  
  f<- ggplot(data =hplc, aes(y=si,x= co)) 
 ff<- f+geom_point(col="green", size=3)+  geom_smooth(method="lm", col="firebrick")+
    ggtitle("Curva de calibración de Siringaldehído ", subtitle="Cromatografía de Líquidos de Alta Resolución") +
    xlab("Concentración (ppm)") + ylab("Área (mAU)")+
    coord_cartesian(xlim=c(0,10), ylim=c(0, max(hplc[,7])))+
    theme(panel.background = element_rect(fill = 'grey'),
          panel.grid.major = element_line(colour = "burlywood", size=1.5),
          panel.grid.minor = element_line(colour = "tomato", 
                                          size=.25, 
                                          linetype = "dashed"),
          panel.border = element_blank(),
          axis.line.x = element_line(colour = "darkorange", 
                                     size=1.5, 
                                     lineend = "butt"),
          axis.line.y = element_line(colour = "darkorange", 
                                     size=1.5))  
  
  
  
  multiplot(aa, bb, cc, dd, ee, ff, cols=2)
grid.arrange(aa, bb, cc, dd, ee, ff, nrow=3)  



grid.arrange(
  aa,
  bb,
  cc,
  dd,
  nrow = 2,
  top = "Curvas de calibración en Cromatografía de Líquidos de Alta Resolución (HPLC)",
  bottom = textGrob(
    "this footnote is right-justified",
    gp = gpar(fontface = 3, fontsize = 9),
    hjust = 1,
    x = 1
  )
)





fit <- lm(hmf5~ co, data = hplc)
ggplotRegression <- function (fit) {
  
  ggplot(fit$model, aes_string(x = names(fit$model)[2], y = names(fit$model)[1])) + 
    geom_point() +
    stat_smooth(method = "lm", col = "red") +
    labs(title = paste("Adj R2 = ",signif(summary(fit)$adj.r.squared, 5),
                       "Intercept =",signif(fit$coef[[1]],5 ),
                       " Slope =",signif(fit$coef[[2]], 5),
                       " P =",signif(summary(fit)$coef[2,4], 5)))+
   # ggtitle("Curva de calibración de Siringaldehído ", subtitle="Cromatografía de Líquidos de Alta Resolución") +
    xlab("Concentración (ppm)") + ylab("Área (mAU)")+
    coord_cartesian(xlim=c(0,10), ylim=c(0, max(hplc[,2])))+
    theme(panel.background = element_rect(fill = 'grey'),
          panel.grid.major = element_line(colour = "burlywood", size=1.5),
          panel.grid.minor = element_line(colour = "tomato", 
                                          size=.25, 
                                          linetype = "dashed"),
          panel.border = element_blank(),
          axis.line.x = element_line(colour = "darkorange", 
                                     size=1.5, 
                                     lineend = "butt"),
          axis.line.y = element_line(colour = "darkorange", 
                                     size=1.5)) 

}








fit1 <- lm(hmf5~ co, data = hplc)
ggplotRegression(fit1)
ggplotRegression(lm(a2~ co, data = hplc))







fit <- lm(hmf5 ~ co, data = hplc)
blue.bold.italic.16.text <- element_text(face = "bold.italic", color = "blue", size = 16)
red.bold.italic.text <- element_text(face = "bold.italic", color = "pink")
equation = function(x) {
  lm_coef <- list(a = round(coef(x)[1], digits = 2),
                  b = round(coef(x)[2], digits = 2),
                  r2 = round(summary(x)$r.squared, digits = 5));
  lm_eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(R)^2~"="~r2,lm_coef)
  as.character(as.expression(lm_eq));                 
}


p11 <- ggplot(hplc, aes(x=co, y=hmf5)) + geom_point(color="red",
                                                    fill="blue",
                                                    shape=21,
                                                    alpha=0.5,
                                                    size=4,
                                                    stroke = 2) + geom_smooth(method=lm, col="springgreen4") +
  ggtitle("Curva de calibración 5 Hidroximetil Furfural ", subtitle="Cromatografía de Líquidos de Alta Resolución") +
  scale_x_continuous(name = "Concentración (ppm)") +
  scale_y_continuous(name = "Área (mUA)") +
  coord_cartesian(xlim=c(0,10), ylim=c(0, max(hplc[,2])))+
  theme(panel.background = element_rect(fill = 'gray94'),
        panel.grid.major = element_line(colour = "azure", size=1.5),
        panel.grid.minor = element_line(colour = "tomato", 
                                        size=.25, 
                                        linetype = "dashed"),
        panel.border = element_blank(),
        axis.line.x = element_line(colour = "blue", 
                                   size=1.5, 
                                   lineend = "butt"),
        axis.line.y = element_line(colour = "blue", 
                                   size=1.5), 
        axis.text.x = element_text(face="bold", color="#993333", 
                                                                         size=14, angle=45),
        axis.text.y = element_text(face="bold", color="#993333", 
                                   size=14, angle=45),
        title = red.bold.italic.text, axis.title = blue.bold.italic.16.text) +
  annotate("rect", xmin = 4.5, xmax = 7.5, ymin = 42000, ymax = 65000, fill="white", colour="red") +
  annotate("text", x = 6, y = 60000, label = equation(fit), parse = TRUE)
p11



