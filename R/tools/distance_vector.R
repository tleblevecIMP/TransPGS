#' Computes a real distance vector from the number of pixels and the real distance pixel ratio
#'

distance_vector<-function(studied_pixels,real_length,total_pixel){
  distance = numeric(studied_pixels)
  for ( h in seq(studied_pixels)){
    distance[h]= h*real_length/total_pixel
  }
  return(distance)
}
