cross_val_lm = function(fo, data, num_folds){
  # calcula el error de cross validation para regresion lineal
  #
  # fo: formula
  # data: data.frame de con los datos
  # num_folds: numero de folds
  # 
  # version 2018.1
  
  # nos aseguramos de que fo sea una formula
  fo = as.formula(fo)
  
  # numero total de datos
  n = nrow(data)
  
  # posiciones de train y test
  pos = cross_val_pos(n,num_folds)

  mse_k = rep(0,num_folds)
  for (k in 1:num_folds){
    # datos de training y de validation de cada fold
    datos_train = data[pos$train[[k]],]
    datos_test = data[pos$test[[k]],]
    
    # estimamos el modelo
    mk = lm(fo, data = datos_train)
    
    # error de prediccion
    y_name = all.vars(fo)[1]
    y_test = datos_test[,y_name]
    y_test_p = predict(mk,datos_test)
    
    mse_k[k] = MSE(y_test,y_test_p)
  }
  mse_total = mean(mse_k)
  
  return(mse_total)
  
}