rm(list=ls())

d = read.table("../datos_originales/AmesHousing.txt", header = T, sep = "\t")

plot(d$Gr_Liv_Area, d$SalePrice)
atipicos = d$Gr_Liv_Area > 4000
atipicos_pos = which(atipicos)
d1 = d[-atipicos_pos,]
plot(d1$Gr_Liv_Area, d1$SalePrice)

set.seed(3)
filas = 1:nrow(d)
filas1 = filas[-atipicos_pos] # quiero mantener los atipicos en los datos
test = sample(filas1,200)
d_test = d[test,]
d_test$Order = 1:nrow(d_test)
d_train = d[-test,]
d_train$Order = 1:nrow(d_train)
d_test1 = d_test[,1:81] # elimino variable "SalePrice"
write.csv(d_train,"AmesHousing1.csv") # datos para trabajar
write.csv(d_test1,"AmesHousing2.csv") # datos para calcular la prediccion de "SalePrice"
write.csv(d_test,"AmesHousing3.csv") # datos reales de "SalePrice" correspondientes a los datos2




