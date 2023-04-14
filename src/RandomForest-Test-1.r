rm(list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection
#install.packages("randomForest") # Install the package
library(randomForest) 
require("data.table")
#Aqui se debe poner la carpeta de la computadora local
setwd("C:\\Users\\marco\\Dropbox\\Austral\\labo12023")   #Establezco el Working Directory
#cargo los datos
dataset  <- fread("./datasets/dataset_pequeno.csv")
dtrain  <- dataset[ foto_mes==202107 ][!is.na(clase_ternaria)]  #defino donde voy a entrenar
#dtrain  <- dtrain[, clase_ternaria := ifelse(clase_ternaria != "BAJA+2", 0, 1)]
dtrain <- dtrain[,clase_ternaria := factor(clase_ternaria, exclude = NULL)]
dapply  <- dataset[ foto_mes==202109 ]  #defino donde voy a aplicar el modelo

# Impute missing values to the mean for numerical variables
for (i in which(sapply(dtrain, is.numeric))) {
  dtrain[is.na(get(names(dtrain)[i])), (names(dtrain)[i]) := mean(dtrain[[i]], na.rm = TRUE)]
}

rf_model <- randomForest(clase_ternaria ~ . , 
                         data = dtrain, 
                         importance = TRUE,
                         type = "categorical", 
                          ntree = 2)

# Use the model to make predictions on the testing data
prediccion <- predict(rf_model, dapply)

summary(prediccion)

#agrego a dapply una columna nueva que es la probabilidad de BAJA+2
dapply[ , prob_baja2 := prediccion[, "BAJA+2"] ]

#solo le envio estimulo a los registros con probabilidad de BAJA+2 mayor  a  1/40
dapply[ , Predicted := as.numeric( prob_baja2 > 1/30 ) ]

#genero el archivo para Kaggle
#primero creo la carpeta donde va el experimento
dir.create( "./exp/" )
dir.create( "./exp/MA2001" )

fwrite( dapply[ , list(numero_de_cliente, Predicted) ], #solo los campos para Kaggle
        file= "./exp/MA2001/KM101_001.csv",
        sep=  "," )

