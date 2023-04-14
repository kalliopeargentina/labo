#install.packages("philentropy")
rm(list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection
library(philentropy)
require("data.table")
#Aqui se debe poner la carpeta de la computadora local
setwd("C:\\Users\\marco\\Dropbox\\Austral\\labo12023")   #Establezco el Working Directory
#cargo los datos
dataset  <- fread("./datasets/dataset_pequeno.csv")
vi_fini07 <- na.omit(dataset[foto_mes==202107]$ctrx_quarter)
vi_fini09 <- na.omit(dataset[foto_mes==202109]$ctrx_quarter)
pdf07 <- density(vi_fini07)
pdf09 <- density(vi_fini09)
jsd <- JSD(rbind(pdf07$y, pdf09$y))
threshold <- 0.1
if(jsd >= threshold) {
  cat("Data drift detectado!\n")
} else {
  cat("Data drift no detectado.\n")
}



library(ggplot2)

plot(density(vi_fini07), main = "Superposición de densidad")
lines(density(vi_fini09), col = "red")
legend("topright", legend = c("Master_finicimora 07",
 "Master_finicimora 07"), col = c("black", "red"),
  lty = 1)