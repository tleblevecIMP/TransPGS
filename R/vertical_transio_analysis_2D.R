#' importing the images and computing transiograms
#' @param images is a list made of the different path files for the images
#' the length of the list correponds to the number of facies
#'
library("bmp")
library("RGeostats")

vertical_transio_analysis_2D<-function(images,vert_length,transio_length){

  windows()
  facies <-vector("list",length(images))
  # reading images
  for ( i in 1:length(images)){
    f<-read.bmp(images[[i]])
    f[f != 0]<-1
    facies[[i]]<-f
  }
  vert_transio_matrix(length(images),facies,vert_length,nrow(facies[[1]]),transio_length)
}
