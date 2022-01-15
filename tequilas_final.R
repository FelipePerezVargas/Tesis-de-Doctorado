rm(list=ls())
library(ggplot2)
library(plotly)
library(ggthemes)
library(extrafont)
library(grid)
library(gridExtra)
library(gapminder)


tb9 <- read.table("/Users/Admin/Downloads/Respaldo_recuperación_tesis/M1.txt", header=TRUE, sep="", na.strings="NA", dec=".", strip.white=TRUE)


plot(ut[,1], ut[,2])
