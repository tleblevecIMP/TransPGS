#' this function computes the cross correlations for a distance h 
#' using a gaussian covariance


coregionalization_model_gauss<-function(h,r1,r2,rho,shift,rho1a){
  
  rho1h=gaussian_cov(r1,h)
  rho2h = ((rho^2)/(rho1a^2)) * rho1h + (1-((rho^2)/(rho1a^2))) * gaussian_cov(r2,h)
  
  return (c(rho1h,rho2h,rho1a))
}