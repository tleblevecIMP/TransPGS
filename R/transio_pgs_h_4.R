#' to compute the transiograms with four facies
#'

transio_pgs_h_4<-function(pF1,pF2,pF3,a,b,rho1a,cor_h,rho,shift,h){
  pF4 = 1-pF3-pF2-pF1
  rho1h = cor_h[1]
  rho2h = cor_h[2]
  rho12h =  cor_h[3]
  rho21h =  cor_h[4]

  sigma = matrix(c(1,rho1h,rho,rho12h,rho1h,1,rho21h,rho,rho,rho21h,1,rho2h,rho12h,rho,rho2h,1),4,4)
  t11<- pmvnorm(lower=c(-Inf,-Inf,b,b),upper=c(a,a,Inf,Inf),mean = c(0,0,0,0),sigma=sigma)/pF1
  t12<- pmvnorm(lower=c(-Inf,a,b,b),upper=c(a,Inf,Inf,Inf),mean = c(0,0,0,0),sigma=sigma)/pF1
  t13<- pmvnorm(lower=c(-Inf,a,b,-Inf),upper=c(a,Inf,Inf,b),mean = c(0,0,0,0),sigma=sigma)/pF1
  t14<- pmvnorm(lower=c(-Inf,-Inf,b,-Inf),upper=c(a,a,Inf,b),mean = c(0,0,0,0),sigma=sigma)/pF1
  t21<- pmvnorm(lower=c(a,-Inf,b,b),upper=c(Inf,a,Inf,Inf),mean = c(0,0,0,0),sigma=sigma)/pF2
  t22<- pmvnorm(lower=c(a,a,b,b),upper=c(Inf,Inf,Inf,Inf),mean = c(0,0,0,0),sigma=sigma)/pF2
  t23<- pmvnorm(lower=c(a,a,b,-Inf),upper=c(Inf,Inf,Inf,b),mean = c(0,0,0,0),sigma=sigma)/pF2
  t24<- pmvnorm(lower=c(a,-Inf,b,-Inf),upper=c(Inf,a,Inf,b),mean = c(0,0,0,0),sigma=sigma)/pF2
  t31<- pmvnorm(lower=c(a,-Inf,-Inf,b),upper=c(Inf,a,b,Inf),mean = c(0,0,0,0),sigma=sigma)/pF3
  t32<- pmvnorm(lower=c(a,a,-Inf,b),upper=c(Inf,Inf,b,Inf),mean = c(0,0,0,0),sigma=sigma)/pF3
  t33<- pmvnorm(lower=c(a,a,-Inf,-Inf),upper=c(Inf,Inf,b,b),mean = c(0,0,0,0),sigma=sigma)/pF3
  t34<- pmvnorm(lower=c(a,-Inf,-Inf,-Inf),upper=c(Inf,a,b,b),mean = c(0,0,0,0),sigma=sigma)/pF3
  t41<- pmvnorm(lower=c(-Inf,-Inf,-Inf,b),upper=c(a,a,b,Inf),mean = c(0,0,0,0),sigma=sigma)/pF4
  t42<- pmvnorm(lower=c(-Inf,a,-Inf,b),upper=c(a,Inf,b,Inf),mean = c(0,0,0,0),sigma=sigma)/pF4
  t43<- pmvnorm(lower=c(-Inf,a,-Inf,-Inf),upper=c(a,Inf,b,b),mean = c(0,0,0,0),sigma=sigma)/pF4
  t44<- pmvnorm(lower=c(-Inf,-Inf,-Inf,-Inf),upper=c(a,a,b,b),mean = c(0,0,0,0),sigma=sigma)/pF4

  return (c(t11,t12,t13,t14,t21,t22,t23,t24,t31,t32,t33,t34,t41,t42,t43,t44))
}
