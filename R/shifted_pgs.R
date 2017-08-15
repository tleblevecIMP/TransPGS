#' performs a shifted pgs with rgeostat
#'

shifted_pgs<-function(nx,ny,dx,dy,r1,an1,r2,an2,seed,rho,a,b,shift){

  #infering
  shift_cell = trunc(shift/dx)
  model1<-gaussian_cova_rgeos(r1,an1)
  model2<-gaussian_cova_rgeos(r2,an2)
  rho1a= model.eval(model1,shift,as.cov=TRUE)

  #simulation
  grid<-gaussian_field_rgeostat(nx+shift_cell,ny,dx,dy,model1,model2,seed)
  facies<- shifted_trunc(grid[,4],grid[,5],rho,a,b,rho1a,shift_cell)
  grid<-db.add(grid,facies)

  return(grid)
}
