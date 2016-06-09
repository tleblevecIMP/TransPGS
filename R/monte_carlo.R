#' compare the transio model to the experimental transio computed on the simulations
#' @param dist_max is the maximal distance at which we want to compute the transiogram
#' @param n is the number of simulations
#' @param ncell is the number of cell on the section
#' @param xdim is the size of every cell

monte_carlo<-function(ncell,n,dx,dist_max,pF1,pF2,rho,r1,r2,shift){

  a = qnorm(pF1)
  b<-threshold_fitting(pF1,pF2,rho,1000.)

  result<-transio_pgs(c(pF1,pF2),r1,r2,rho,a,b,shift,dist_max,dx)
  t11<-result[1,]
  t12<-result[2,]
  t21<-result[3,]
  t22<-result[4,]
  distance<-result[5,]


  windows()
  layout(matrix(c(1,2,3,4),2,2,byrow=TRUE))

  # for the auto transition 1
  plot(distance,t11,type="l",col="blue",main="Auto transition of facies 1",xlab="upward distance",ylab="t11(h)",xlim=c(0, dist_max),ylim=c(0.,1.2),lwd=4,cex.axis=1.5,cex.lab=1.5,cex.main=1.5)
  nsimu_transio(n,1,1,ncell,shift,a,b,rho,r1,r2,dx,distance)

  # for the transition 1 2
  plot(distance,t12,type="l",col="blue",main="From facies 1 to Facies 2",xlab="upward distance",ylab="t12(h)",xlim=c(0, dist_max),ylim=c(0,0.9),lwd=4,cex.axis=1.5,cex.lab=1.5,cex.main=1.5)
  nsimu_transio(n,1,2,ncell,shift,a,b,rho,r1,r2,dx,distance)
  # for the transition 2 1
  plot(distance,t21,type="l",col="blue",main="From Facies 2 to Facies 1",xlab="upward distance",ylab="t21(h)",xlim=c(0, dist_max),ylim=c(0,0.9),lwd=4,cex.axis=1.5,cex.lab=1.5,cex.main=1.5)
  nsimu_transio(n,2,1,ncell,shift,a,b,rho,r1,r2,dx,distance)
  # for the auto transition 2
  plot(distance,t22,type="l",col="blue",main="Auto transition of facies 2",xlab="upward distance",ylab="t22(h)",xlim=c(0, dist_max),ylim=c(0.,1.2),lwd=4,cex.axis=1.5,cex.lab=1.5,cex.main=1.5)
  nsimu_transio(n,2,2,ncell,shift,a,b,rho,r1,r2,dx,distance)
}


