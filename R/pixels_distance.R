#' Compute the number of pixels of a distance given the scale pixel/length
#'


pixels_distance<-function(n_pixel,real_length,study_length){
  pix = round(n_pixel*study_length/real_length)
  return (pix)
}
