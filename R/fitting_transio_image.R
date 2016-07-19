#' the whole step for fitting transiograms on an image
#'
fitting_transio_image_vert<-function(images,vert_length,transio_length){
  props<-Proportion_2D_image(images)
  data<-vertical_transio_analysis_2D(images,vert_length,transio_length)
  gaussian_transio_interact(props,transio_length,transio_length/ncol(data),data)
}
