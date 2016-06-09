#' performs a shifted pgs with rgeostat
#'

shifted_pgs<-function(size,dx,r1,r2,seed,rho,a,b,shift){

  #infering
  shift_cell = trunc(shift/dx)
  model1<-gaussian_cova_rgeos(r1)
  model2<-gaussian_cova_rgeos(r2)
  rho1a= model.eval(model1,shift,as.cov=TRUE)

  #simulation
  grid<-gaussian_field_rgeostat(n_cell+shift_cell,dx,model1,model2,seed)
  facies<- shifted_trunc(grid[,4],grid[,5],rho,a,b,rho1a,shift_cell)
  grid<-db.add(grid,facies)

  return(grid)
}
