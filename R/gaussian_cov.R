#' functions that compute the value of the gaussian covariance for a distance  
#' we use the non-centered covariance

gaussian_cov<-function(r1,distance){
  return (exp(-(distance/r1)^2))
}