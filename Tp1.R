datos<-read.csv("data.csv",sep="\t")
n<-nrow(datos) #cantidad de datos
set.seed(123)
#separacion 70%, 30% de los datos
data_training<-datos[sample(1:n, size = 0.7 * n),]
data_set<-datos[-sample(1:n, size = 0.7 * n),]
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
  res=0
  n<-length(reales)
  for(i in 1:n){
    res=res+abs(predichos[i]-reales[i])
  }
  return(res/n)
  
}
modelo_Q9$coefficients
