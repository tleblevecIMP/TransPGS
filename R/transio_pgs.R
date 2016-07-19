#' function to compute the theoretical transiograms for a set of distances from PGS
#' @param gauss defines if a gaussian variogram is used. In that case we use the known expression
#' of the gaussian variogram ,if not we use rgeostat
#'

transio_pgs<-function(props,r1,r2,rho,a,b,shift,dist_max,dx){

  pF1 = props[1]
  pF2 = props[2]
  pF3 = props[3]
  # rho1a is computed here and will be used for the other computations
  rho1a = gaussian_cov(r1,shift)

  size=trunc(dist_max/dx)
  t11<-rep(1,size)
  t12<-rep(0,size)
  t13<-rep(0,size)
  t21<-rep(0,size)
  t22<-rep(1,size)
  t23<-rep(0,size)
  t31<-rep(0,size)
  t32<-rep(0,size)
  t33<-rep(1,size)
  distance<-rep(0,size)
  i=1

  for ( h in seq(dx,dist_max,dx)){
    i =i+1
    distance[i]<-h
    cor_h<-coregionalization_model_gauss(h,r1,r2,rho,shift,rho1a)
    transios <- transio_pgs_h(pF1,pF2,a,b,rho1a,cor_h,rho,shift,h)
    t11[i]<- transios[1]
    t12[i]<- transios[2]
    t13[i]<-1-t11[i]-t12[i]
    t21[i]<- transios[3]
    t22[i]<- transios[4]
    t23[i]<-1-t21[i]-t22[i]
    t31[i]<-(pF1-t11[i]*pF1-t21[i]*pF2)/pF3
    t32[i]<-(pF2-t12[i]*pF1-t22[i]*pF2)/pF3
    t33[i]<-(pF3-t13[i]*pF1-t23[i]*pF3)/pF3
  }

  return(rbind(t11,t12,t13,t21,t22,t23,t31,t32,t33,distance))
}
