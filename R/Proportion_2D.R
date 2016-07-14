#'Compute the proportions of a 2d dataset coded in a list of matrix
#'


proportion_2D<-function(facies,nfacies){
  prop<-rep(0,nfacies)
  for( i in seq(nfacies)){
    facies<-binarize_facies(facies,i)
    prop[i]=mean(facies,na.rm=TRUE)
  }
  #prop<-prop_correction_NA(prop)
  return ( prop)
}
