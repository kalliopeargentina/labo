rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection
require("data.table")
#Aqui se debe poner la carpeta de la computadora local
setwd("C:\\Users\\marco\\Dropbox\\Austral\\labo12023")   #Establezco el Working Directory
#cargo los datos
dataset  <- fread("./datasets/dataset_pequeno.csv")
vi_fini07 <- na.omit(dataset[foto_mes==202107]$ctrx_quarter)
vi_fini09 <- na.omit(dataset[foto_mes==202109]$ctrx_quarter)

ks.test(vi_fini07,vi_fini09)
ks.test(sample(vi_fini07, 1000, replace = TRUE), sample(vi_fini09, 1000, replace = TRUE))


library(ggplot2)

plot(density(vi_fini07), main = "Superposición de densidad")
lines(density(vi_fini09), col = "red")
legend("topright", legend = c("Master_finicimora 07",
 "Master_finicimora 07"), col = c("black", "red"),
  lty = 1)