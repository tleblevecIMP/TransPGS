#' Compute the proportions from a set of 2D images describing facies
#' The proportion is not straightforward because some values can be non assigned
#' Therefore all the facies are required and a correction is applied for the no value cells

Proportion_2D<-function(images){
  nfacies <- length(images)
  facies <- reading_facies_images(images)
  prop<-rep(0,nfacies)
  for( i in seq(nfacies)){
    prop[i]=mean(facies[[i]])
  }
  # correction for the non assigned values:
  prop <- prop_correction_NA(prop)

  return ( prop)
}
