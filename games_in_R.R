#install.packages ("devtools",dependencies = T) 
library(devtools)
devtools::install_github("RLesur/Rcade")
install.packages("yaml",dependencies = T)
library(Rcade)

# Listar juegos disponibles
Rcade::games

# Jugar juego 1
Rcade::games$Core

# Jugar juego 2
Rcade::games$Mariohtml5

s# Jugar juego 3
Rcade::games$Pacman

# Jugar juego 4
Rcade::games$SpiderSolitaire

#bold <b>No</b>


lon<-fur[,1]
furan<-as.numeric(furan)
af<-as.numeric(af)
me<-as.numeric(me)
ef<-data.frame(lon, furan, af, me,  group=c('Longitud', 'Furfural', '5-Metil Furfural', '5-Metil Furfrual'))
fut <- list(family = "Courier New, monospace",size = 18,color = "#7f7f7f")
xe <- list(title = "Longitud de Onda (nm)", titlefont = fut, linecolor = toRGB("black"),linewidth = 3)
ye <- list(title = "Absorbancia (UA)",titlefont = fut, linecolor = toRGB("black"),linewidth = 3)
et <- plot_ly(ef, x = ~lon, y = ~furan, name = 'Furfural', type = 'scatter', mode = 'lines', line= list(color = 'blue', width = 2)) %>%
  add_trace(y = ~af, name = '2-Acetil Furfural', mode = 'lines',  line= list(color = 'red', width = 2)) %>%
  add_trace(y = ~me, name = '5-Metil Furfural', mode = 'lines', line= list(color = 'green', width = 2))%>% 
  layout(title="Coeficientes de Aboserción Molar Furfurales", xaxis = xe, yaxis = ye);et



