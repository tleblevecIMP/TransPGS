#' simulates gaussian fields with rgeostat on a grid with gaussian variogram
#'

gaussian_field_rgeostat<-function(size,dx,model1,model2,seed){
  neigh=new("neigh",ndim=2,type=0);
  grid<-db.create(flag.grid=TRUE,x0=c(0,0),nx=c(size,1),dx=c(dx,1))
  grid<-simtub(dbout=grid,model=model1,neigh=neigh,seed=seed,nbsimu=1)
  grid<-simtub(dbout=grid,model=model2,neigh=neigh,seed=seed+1,nbsimu=1)
  return(grid)
}
