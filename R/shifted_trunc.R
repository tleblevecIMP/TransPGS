#' shifted truncation from two gaussian fields, equations from Armstrong, 2011
#' we use the function shift from the package binhf
#' @param a is here the truncation of Z1 and not the shift

library("binhf")

shifted_trunc<-function(Y1,Y2,rho,a,b,rho1a,shift_cell){
  # Z2 computation
  shifted_Y1<-shift(Y1,shift_cell,"left")
  Z2<-(rho/rho1a)*shifted_Y1+sqrt(1-(rho^2)/(rho1a^2))*Y2

  # truncation
  facies<- rep(3,length=length(Y2)) # initialization with facies 3
  facies[Y1<a]<-1
  facies[(Y1>a)&(Z2>b)]<-2

  return(facies)
}
