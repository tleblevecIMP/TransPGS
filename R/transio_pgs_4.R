#' compute transiograms when four facies
#'

transio_pgs_4<-function(props,r1,r2,rho,a,b,shift,dist_max,dx){

  pF1 = props[1]
  pF2 = props[2]
  pF3 = props[3]
  # rho1a is computed here and will be used for the other computations
  rho1a = gaussian_cov(r1,shift)

  size=trunc(dist_max/dx)
  t11<-rep(1,size)
  t12<-rep(0,size)
  t13<-rep(0,size)
  t14<-rep(0,size)
  t21<-rep(0,size)
  t22<-rep(1,size)
  t23<-rep(0,size)
  t24<-rep(0,size)
  t31<-rep(0,size)
  t32<-rep(0,size)
  t33<-rep(1,size)
  t34<-rep(0,size)
  t41<-rep(0,size)
  t42<-rep(0,size)
  t43<-rep(0,size)
  t44<-rep(1,size)
  distance<-rep(0,size)
  i=1

  for ( h in seq(dx,dist_max,dx)){
    i =i+1
    distance[i]<-h
    cor_h<-coregionalization_model_gauss(h,r1,r2,rho,shift,rho1a)
    transios <- transio_pgs_h_4(pF1,pF2,pF3,a,b,rho1a,cor_h,rho,shift,h)
    t11[i]<- transios[1]
    t12[i]<- transios[2]
    t13[i]<- transios[3]
    t14[i]<- transios[4]
    t21[i]<- transios[5]
    t22[i]<- transios[6]
    t23[i]<- transios[7]
    t24[i]<- transios[8]
    t31[i]<- transios[9]
    t32[i]<- transios[10]
    t33[i]<- transios[11]
    t34[i]<- transios[12]
    t41[i]<- transios[13]
    t42[i]<- transios[14]
    t43[i]<- transios[15]
    t44[i]<- transios[16]
  }

  return(rbind(t11,t12,t13,t14,t21,t22,t23,t24,t31,t32,t33,t34,t41,t42,t43,t44,distance))
}
