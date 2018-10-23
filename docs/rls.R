rls <- function(y,x){
  # 
  # esta funcion estima los parámetros de
  # un modelo de regresión lineal simple
  #
  # javier.cara@upm.es, version 2018.09
  # -----------------------------------------------------------------------
  
  n = length(x)
  
  beta1_e = cov(x,y)/var(x)
  beta0_e = mean(y) - beta1_e*mean(x)
  
  e = y - (beta0_e + beta1_e*x)
  sigma_e = sqrt(sum(e^2)/(n-2))
  
  salida = list(beta0_e = beta0_e, beta1_e = beta1_e, sigma_e = sigma_e)
  
  return(salida)
}
