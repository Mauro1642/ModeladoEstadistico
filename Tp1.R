library(MASS)
datos<-read.csv("data.csv",sep="\t")
n<-nrow(datos) #cantidad de datos
set.seed(123)
#separacion 70%, 30% de los datos
data_training<-datos[sample(1:n, size = 0.7 * n),]
data_test<-datos[-sample(1:n, size = 0.7 * n),]
data_training$Q2 <- ordered(data_training$Q2, levels = c(1, 2, 3, 4, 5)) #lo convierto en un factor ordenado
modelo_Q2 <- polr(Q2 ~ age, data = data_training, method = "logistic", Hess = TRUE)
#Pregunta 6
data_training$Q9<-ordered(data_training$Q9, levels = c(1, 2, 3, 4, 5)) #lo convierto en un factor ordenado
modelo_Q9 <- polr(Q9 ~ age, data = data_training, method = "logistic", Hess = TRUE)
proba<-function(theta,coef,age,k){
  denominador<-1+exp(-(theta[k]-coef*age))
  return (1/denominador)
}
#Obtengo los parametros entrenados y luego calculo la probabilidad
theta<-modelo_Q9$zeta
coeficiente<-modelo_Q9$coefficients
k<-3
age<-25
print(1-proba(theta,coeficiente,age,k))
#Pregunta 7
loss_function <- function(predichos, reales) {
  pred_num <- as.numeric(as.character(predichos)) #convierto los factores ordenados a numericos
  real_num <- as.numeric(as.character(reales))
  
  n <- length(real_num)
  res <- sum(abs(pred_num - real_num)) / n
  return(res)
}
#pregunta 8
data_training$Q2 <- as.numeric(as.character(data_training$Q2))
modelo_lineal <- lm(Q2 ~ age, data = data_training)
#predigo para el modelo lineal
predicciones_continuas <- predict(modelo_lineal, newdata = data_test)
predicciones_continuas
predicciones_finales <- pmin(pmax(round(predicciones_continuas), 1), 5)
predicciones_finales
L_linear<-loss_function(predicciones_finales,data_test$Q2)
#predigo para el modelo ordinal
predicciones_ordinal<-predict(modelo_Q2,newdata=data_test)
L_ordinal<-loss_function(predicciones_ordinal,data_test$Q2)
predicciones_ordinal
