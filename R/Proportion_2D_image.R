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
