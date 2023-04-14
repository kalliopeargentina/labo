#install.packages("PDtoolkit")
library(PDtoolkit)

rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection
require("data.table")
#Aqui se debe poner la carpeta de la computadora local
setwd("C:\\Users\\marco\\Dropbox\\Austral\\labo12023")   #Establezco el Working Directory
#cargo los datos
dataset  <- fread("./datasets/dataset_pequeno.csv")


vi_fini07 <- na.omit(dataset[foto_mes==202107]$ctrx_quarter)
vi_fini09 <- na.omit(dataset[foto_mes==202109]$ctrx_quarter)

psi_res <- psi(base = vi_fini07, target = vi_fini09, bin = 10, alpha = 0.05)

psi_res$res$psi

library(ggplot2)
# Calculate the bandwidths for each dataset
#bw1 <- 1.06 * sd(vi_fini07) / length(vi_fini07)^0.2
#bw2 <- 1.06 * sd(vi_fini09) / length(vi_fini09)^0.2
# Plot the densities of the two datasets
plot(density(vi_fini07 ), main = "Overlapping Densities")
lines(density(vi_fini09), col = "red")
legend("topright", legend = c("Dataset 1", "Dataset 2"), col = c("black", "red"), lty = 1)