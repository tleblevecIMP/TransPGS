#' computing the transiograms for a distance h
#' we use here gaussian variograms

#' @param r1 is the range of the first gaussian variogram

library(mvtnorm)



transio_pgs_h<-function(pF1,pF2,a,b,rho1a,cor_h,rho,shift,h){

  rho1h = cor_h[1]
  rho2h = cor_h[2]
  rho12h =  cor_h[3]
  rho21h =  cor_h[4]


  t11<- pmvnorm(lower=c(-Inf,-Inf),upper=c(a,a),mean = c(0,0),sigma=matrix(c(1,rho1h,rho1h,1),2,2))/pF1
  sigma <- matrix(c(1,rho1h,rho12h,rho1h,1,rho,rho12h,rho,1),3,3)
  t12<- pmvnorm(lower=c(-Inf,a,b),upper=c(a,Inf,Inf),mean = c(0,0,0),sigma=sigma)/pF1
  sigma <- matrix(c(1,rho,rho1h,rho,1,rho21h,rho1h,rho21h,1),3,3)
  t21<- pmvnorm(lower=c(a,b,-Inf),upper=c(Inf,Inf,a),mean = c(0,0,0),sigma=sigma)/pF2
  sigma <- matrix(c(1,rho1h,rho,rho12h,rho1h,1,rho21h,rho,rho,rho21h,1,rho2h,rho12h,rho,rho2h,1),4,4)
  t22<- pmvnorm(lower=c(a,a,b,b),upper=c(Inf,Inf,Inf,Inf),mean = c(0,0,0,0),sigma=sigma)/pF2

  return (c(t11,t12,t21,t22))
}
