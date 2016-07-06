#' importing the images and computing transiograms
#' @param images is a list made of the different path files for the images
#' the length of the list correponds to the number of facies
#'
library("bmp")
library("RGeostats")

vertical_transio_analysis_2D<-function(images,vert_length,transio_length){

  windows()
  facies <- reading_facies_images(images)
  vert_transio_matrix(length(images),facies,vert_length,nrow(facies[[1]]),transio_length)
}
