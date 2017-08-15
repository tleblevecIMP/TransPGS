# computes the transiogram at every lag from the coregionalization model with three correlated gaussian functions

transio_pgs_h_4F_3G<-function(truncs,ranges,lambdas,h){

  # the transiograms are defined by a tree
  sigma11 = matrix(c(1,rho1z,rho1z,1),2,2)
  t11<- pmvnorm(lower=c(-Inf,-Inf),upper=c(trunc[1],trunc[1]),mean = c(0,0),sigma=sigma11)/pF1
  sigma12 = matrix(c(1,rho1z,))
  sigma22 = matrix(c(1,rho1z,rho1z,1),4,4)
  t11<- pmvnorm(lower=c(-Inf,-Inf),upper=c(trunc[1],trunc[1]),mean = c(0,0),sigma=sigma11)/pF1

}
