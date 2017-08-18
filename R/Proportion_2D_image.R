#' Compute the proportions from a set of 2D images describing facies
#' The proportion is not straightforward because some values can be non assigned
#' Therefore all the facies are required and a correction is applied for the no value cells
#' @param prop is a vector of proportion

Proportion_2D_image<-function(images){
  nfacies <- length(images)
  facies <- reading_facies_images(images)
  prop <- proportion_2D(facies,nfacies)
  return ( prop)
}

#'Compute the proportions of a 2d dataset coded in a list of matrix
#'


proportion_2D<-function(facies,nfacies){
  prop<-rep(0,nfacies)
  for( i in seq(nfacies)){
    fi<-binarize_facies(facies,i)
    prop[i]=mean(fi,na.rm=TRUE)
  }
  #prop<-prop_correction_NA(prop)
  return ( prop)
}
