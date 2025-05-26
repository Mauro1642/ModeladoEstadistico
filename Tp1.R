datos<-read.csv("data.csv",sep="\t")
n<-nrow(datos) #cantidad de datos
set.seed(123)
#separacion 70%, 30% de los datos
data_training<-datos[sample(1:n, size = 0.7 * n),]
data_test<-datos[-sample(1:n, size = 0.7 * n),]
library(MASS)
data_training$Q33 <- ordered(data_training$Q33, levels = c(1, 2, 3, 4, 5))
modelo_Q33 <- polr(Q33 ~ age, data = data_training, method = "logistic", Hess = TRUE)
#Pregunta 6
data_training$Q9<-ordered(data_training$Q9, levels = c(1, 2, 3, 4, 5))
modelo_Q9 <- polr(Q9 ~ age, data = data_training, method = "logistic", Hess = TRUE)
proba<-function(theta,coef,age,k){
  denominador<-1+exp(-(theta[k]-coef*age))
  return (1/denominador)
}
theta<-modelo_Q9$zeta
coeficiente<-modelo_Q9$coefficients
k<-4
age<-25
print(proba(theta,coeficiente,age,k))
#Pregunta 7
loss_function<-function(predichos,reales) {
  pred_num <- as.numeric(predichos)
  real_num <- as.numeric(reales)
  res=0
  n<-length(real_num)
  for(i in 1:n){
    res=res+abs(pred_num[i]-real_num[i])
  }
  return(res/n)
  
}
#pregunta 8
data_training$Q_num <- as.numeric(as.character(data_training$Q33))
modelo_lineal <- lm(Q_num ~ age, data = data_training)
#predigo para el modelo lineal
predicciones_continuas <- predict(modelo_lineal, newdata = data_test)
predicciones_finales <- pmin(pmax(round(predicciones_continuas), 1), 5)
L_linear<-loss_function(predicciones_finales,data_test$Q33)
#predigo para el modelo ordinal
data_test$Q33 <- factor(data_test$Q33,
                        levels = c("1", "2", "3", "4", "5"),
                        ordered = TRUE)
predicciones_ordinal<-predict(modelo_Q33,newdata=data_test)
levels(predicciones_ordinal)
levels(data_test$Q33)
L_ordinal<-loss_function(predicciones_ordinal,data_test$Q33)
