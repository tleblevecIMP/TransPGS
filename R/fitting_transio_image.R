#' the whole step for fitting transiograms on an image
#'
fitting_transio_image_vert<-function(images,vert_length,hor_length,vert_transio_length,hor_transio_length){
  facies<-reading_facies_images(images)
  nx = ncol(facies)
  ny = nrow(facies)
  props<-Proportion_2D_image(images)
  data_vert<-vertical_transio_analysis_2D(images,vert_length,vert_transio_length)
  data_hor<-lateral_transio_analysis_2D(images,hor_length,hor_transio_length)
  gaussian_transio_interact(props,vert_transio_length,hor_transio_length,vert_length/nx,hor_length/ny,data_vert,data_hor,nx,ny)
}
