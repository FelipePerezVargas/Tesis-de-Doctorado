rm(list=ls())
library(meme)
library(jpeg)
library(ggplot2)
library(grid)
library(ggimage)
library(micromap)

u <- "https://nerdist.com/wp-content/uploads/2016/04/deadpool-2-featured-image.jpg"

meme(u, "Soy el papu", "de papus!", color="purple")

u <- "http://www.happyfamilyneeds.com/wp-content/uploads/2017/08/angry8.jpg"
meme(u, "code", "all the things!")

mmplot(u) + mm_caption("calm down", "and RTFM", color="purple")




library("devtools")
install_github("leeper/meme")
library("meme")
templates <- meme::get_templates("imgflip")

user <- 'perejil32'#type your user name here (as a string)
pass <- 'neutrino666' #type your password here (as a string)

  exampleMeme <- create_meme(templates[[2]], 
                             "me la pelas", 
                             "you understanding me",
                             username = user,
                             password = pass)
  plot(exampleMeme)
  
  
  
for(i in 1:10){
  #for(i in 1:length(templates)){
  plot(create_meme(templates[[i]], 
                   toString(i), 
                   templates[[i]]$name,
                   username = user,
                   password = pass))
}
  
  
  plot(create_meme(templates[[84]], 
                   "With great power", 
                   "Comes great responsibility",
                   username = user,
                   password = pass))
  
  
 
