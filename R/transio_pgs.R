#' function to compute the theoretical transiograms for a set of distances from PGS
#'
#'

transio_pgs<-function(props,r1,r2,rho,a,b,shift,dist_max,step){

  t11_mod<-rep(1,dist_max)
  t12_mod<-rep(0,dist_max)
  t21_mod<-rep(0,dist_max)
  t22_mod<-rep(1,dist_max)
  distance<-rep(1,dist_max)
  for ( h in 2:dist_max){
    d = (h-1)/20.
    distance[h]<-d
    transios <- transio_pgs_h(props[1],props[2],a,b,r1,r2,rho,shift,d)
    t11[h]<- transios[1]
    t12[h]<- transios[2]
    t21[h]<- transios[3]
    t22[h]<- transios[4]
  }

  return(c(t11,t12,t21,t22,distance))
}
