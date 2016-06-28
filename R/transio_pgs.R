#' function to compute the theoretical transiograms for a set of distances from PGS
#' @param gauss defines if a gaussian variogram is used. In that case we use the known expression
#' of the gaussian variogram ,if not we use rgeostat
#'

transio_pgs<-function(props,r1,r2,rho,a,b,shift,dist_max,dx){

  # rho1a is computed here and will be used for the other computations
  rho1a = gaussian_cov(r1,shift)

  size=trunc(dist_max/dx)
  t11<-rep(1,size)
  t12<-rep(0,size)
  t21<-rep(0,size)
  t22<-rep(1,size)
  distance<-rep(0,size)
  i=1

  for ( h in seq(dx,dist_max,dx)){
    i =i+1
    distance[i]<-h
    cor_h<-coregionalization_model_gauss(h,r1,r2,rho,shift,rho1a)
    transios <- transio_pgs_h(props[1],props[2],a,b,rho1a,cor_h,rho,shift,h)
    t11[i]<- transios[1]
    t12[i]<- transios[2]
    t21[i]<- transios[3]
    t22[i]<- transios[4]
  }

  return(rbind(t11,t12,t21,t22,distance))
}
