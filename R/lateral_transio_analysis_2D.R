#' compute the lateral transiogram matrix on a set of indicator pictures
#'

lateral_transio_analysis_2D<-function(images,length,transio_length){
  windows()
  facies <- reading_facies_images(images)
  mat_t<-transio_matrix(length(images),facies,length,ncol(facies),transio_length,0)
  return(mat_t)
}
