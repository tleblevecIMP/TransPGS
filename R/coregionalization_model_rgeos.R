#' compute the coregionalization model from the rgeos covariance models.
#' 

coregionalization_model_rgeos<-function(h,model1,model2,rho,shift){
  rho1h= model.eval(model1,h,dir=c(1,0),as.cov=TRUE)
  rho1a = model.eval(model1,shift,dir=c(1,0),as.cov=TRUE)
  rho2h = model.eval(model2,h,dir=c(1,0),as.cov=TRUE)
  
  return (c(rho1h,rho2h,rho1a))
}