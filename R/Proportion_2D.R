#'Compute the proportions of a 2d dataset coded in a list of matrix
#'


proportion_2D<-function(facies,nfacies){
  prop<-rep(0,nfacies)
  for( i in seq(nfacies)){
    prop[i]=mean(facies[[i]],na.rm=TRUE)
  }
  prop<-prop_correction_NA(prop)
  return ( prop)
}
