#' creating a gaussian variogram model from rgeostat
#'

gaussian_cova_rgeos<-function(r){
  model<-model.create(ndim=2,nvar=1,vartype=4,aniso.coeffs=c(1,1),range=sqrt(3)*r)
  return(model)
}
