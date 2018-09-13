rls <- function(y,x){
  # regresión lineal simple
  
  n = length(x)
  
  beta1_e = cov(x,y)/var(x)
  beta0_e = mean(y) - beta1_e*mean(x)
  
  e = y - (beta0_e + beta1_e*x)
  sigma_e = sqrt(sum(e^2)/(n-2))
  
  salida = list(x = x, y = y, beta0_e = beta0_e, beta1_e = beta1_e, sigma_e = sigma_e)
  
  return(salida)
  
}
##################################################################################
rls_se <- function(m){
  # standard error del modelo m
  x = m$x
  n = length(x)
  sx2 = sum( (x-mean(x))^2 )/n
  
  beta1_se = sqrt( m$sigma_e^2/(n*sx2) )
  beta0_se = sqrt( m$sigma_e^2/n*(1+mean(x)^2/sx2) )
  
  salida = list(beta0_se = beta0_se, beta1_se = beta1_se)
  
  return(salida)
}
##################################################################################
rls_ic <- function(m,alfa){
  # regresion lineal simple con intervalos de confianza
  
  m1 = rls_se(m)
  n = length(m$x)
  ta = qt(1-alfa/2,n-2)
  
  beta1_1 = m$beta1_e - ta*m1$beta1_se
  beta1_2 = m$beta1_e + ta*m1$beta1_se
  
  beta0_1 = m$beta0_e - ta*m1$beta0_se
  beta0_2 = m$beta0_e + ta*m1$beta0_se
  
  salida = list(beta0_ic = c(beta0_1,beta0_2), beta1_ic = c(beta1_1,beta1_2))
  
  return(salida)
}
#################################################################################
rls_pred <- function(m,xp,alfa){
  # predicción en regresion lineal simple
  # m: modelo estimado con rls
  
  yp = m$beta0_e + m$beta1_e*xp
  
  n = length(m$x)
  RSE = m$sigma_e
  ta = qt(1-alfa/2,n-2)
  sx2 = sum( (x-mean(x))^2 )/n
  vpp = 1/n*(1+(xp - mean(x))^2/sx2)
  mp1 = yp - ta*RSE*sqrt(vpp)
  mp2 = yp + ta*RSE*sqrt(vpp)
  
  salida = list(xp = xp, yp = yp, yp_ic = c(mp1,mp2))
  
  return(salida)
  
}