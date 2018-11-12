library(MASS)
d = Boston
set.seed(3)
test = sample(nrow(d),25)
d_test = d[test,]
d_train = d[-test,]
d_test1 = d_test[,-14] # elimino variable "medv"
write.csv(d_train,"Boston1.csv") # datos para trabajar
write.csv(d_test1,"Boston2.csv") # datos para calcular la prediccion de medv
write.csv(d_test,"Boston3.csv") # datos reales de mdev
