#' function to compute the theoretical transiograms for a set of distances from PGS
#'
#'

transio_pgs<-function(props,r1,r2,rho,a,b,shift,dist_max,dx){

  # initialization according to the first value of the transiogram
  size=trunc(dist_max/dx)
  t11<-rep(1,size)
  t12<-rep(0,size)
  t21<-rep(0,size)
  t22<-rep(1,size)
  distance<-rep(0,size)
  i=1
  # the first distance is 0
  for ( h in seq(dx,dist_max,dx)){
    i =i+1
    distance[i]<-h
    transios <- transio_pgs_h(props[1],props[2],a,b,r1,r2,rho,shift,h)
    t11[i]<- transios[1]
    t12[i]<- transios[2]
    t21[i]<- transios[3]
    t22[i]<- transios[4]
  }

  return(rbind(t11,t12,t21,t22,distance))
}
