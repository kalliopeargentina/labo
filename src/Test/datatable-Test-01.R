rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection
require("data.table")
#Aqui se debe poner la carpeta de la computadora local
setwd("C:\\Users\\marco\\Dropbox\\Austral\\labo12023")   #Establezco el Working Directory
#cargo los datos
dataset  <- fread("./datasets/dataset_pequeno.csv")

ksemillas  <- c(771349, 771359, 771389, 771401, 771403,900007, 900019, 900037, 900061, 900089, 900091, 900103, 900121, 900139, 900143, 900149, 900157, 900161, 900169, 900187) #reemplazar por las propias semillas

#------------------------------------------------------------------------------
#particionar agrega una columna llamada fold a un dataset que consiste en una particion estratificada segun agrupa
# particionar( data=dataset, division=c(70,30), agrupa=clase_ternaria, seed=semilla)   crea una particion 70, 30 

particionar  <- function( data,  division, agrupa="",  campo="fold", start=1, seed=NA )
{
  if( !is.na(seed) )   set.seed( seed )

  bloque  <- unlist( mapply(  function(x,y) { rep( y, x )} ,   division,  seq( from=start, length.out=length(division) )  ) )  

  data[ , (campo) :=  sample( rep( bloque, ceiling(.N/length(bloque))) )[1:.N],
          by= agrupa ]
}

dataset <- particionar( data=dataset, division=c(70,30), agrupa="clase_ternaria", seed=ksemillas[1])  #crea una particion 70, 30

#Armo una tablita para contabilizar por clase_ternaria y fold
frecuencia <- dataset[, .N, by = .(clase_ternaria, fold)]
total <- frecuencia[, .(total = sum(N)), by = "clase_ternaria"]

#Calculo la proporción relativa a la clase_ternaria
proporcion <- merge(frecuencia, total, by = "clase_ternaria")
proporcion[, prop := N / total]

#Uno la tabla de frecuencias con la de proporciones
resultado <- merge(frecuencia, proporcion, by = c("clase_ternaria", "fold"))
resultado <- resultado[,.(clase_ternaria,fold,N.x,prop)]

#Cambio el nombre de una de las columnas y muestro
colnames(resultado)[3] <- "total"
resultado
