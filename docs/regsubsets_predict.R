predict.regsubsets = function(objeto_regsubsets, newdata, id){
  # funcion para predecir con regsubsets
  #
  formu = as.formula(objeto_regsubsets$call[[2]])
  X_mat = model.matrix(formu,newdata)
  coefi = coef(objeto_regsubsets, id)
  X_col = names(coefi)
  pred = X_mat[,X_col] %*% coefi
  return(pred)
}