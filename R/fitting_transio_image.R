#' the whole step for fitting transiograms on an image
#'
fitting_transio_image_vert<-function(images,vert_length,hor_length,transio_length){
  facies<-reading_facies_images(images)
  nx = ncol(facies)
  ny = nrow(facies)
  props<-Proportion_2D_image(images)
  data<-vertical_transio_analysis_2D(images,vert_length,transio_length)
  gaussian_transio_interact(props,transio_length,transio_length/ncol(data),hor_length/ny,data,nx,ny)
}
