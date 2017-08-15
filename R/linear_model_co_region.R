# creating the coregionalization model with three gaussian functions

linear_model_co_region<-function(ranges,rhos,shifts){
  rho1a12 = gaussian_cov(ranges[1],shifts[1])
  rho1a13 = gaussian_cov(ranges[1],shifts[3])
  rho1a1213 = gaussian_cov(ranges[1],abs(shifts[1]-shifts[3]))
  rho2a23 = gaussian_cov(ranges[2],shifts[2])
  lambda12= rhos[1]/rho1a12
  lambda13= rhos[3]/rho1a13
  lambda23 = rhos[2]-lambda13 * lambda12 *rho1a1213
  lambda23 =lambda23 / (sqrt(1-lambda12^2)*rho2a23)

  return(lambda12,lambda23,lambda23)
}
