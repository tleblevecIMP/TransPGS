#' performs a shifted pgs with rgeostat
#'


shifted_pgs2d<-function(nx,ny,dx,dy,r1,an1,r2,an2,seed,rho,a,b,shift){

  #infering
  shift_cell = trunc(shift/dx)
  rho1a= exp(-(shift/r1)^2)

  #simulation
  grid1<-gaussian_field2d(nx+shift_cell,ny,dx,dy,r1,an1,seed)
  grid2<-gaussian_field2d(nx+shift_cell,ny,dx,dy,r2,an2,seed)
  facies<- shifted_trunc2d(grid1,grid2,rho,a,b,rho1a,shift_cell)

  return(facies)
}

shifted_pgs1d<-function(nx,dx,r1,r2,seed,rho,a,b,shift){

  #infering
  shift_cell = trunc(shift/dx)
  rho1a= exp(-(shift/r1)^2)

  #simulation
  grid1<-gaussian_field1d(nx+shift_cell,dx,r1,seed)
  grid2<-gaussian_field1d(nx+shift_cell,dx,r2,seed)
  facies<- shifted_trunc1d(grid1,grid2,rho,a,b,rho1a,shift_cell)

  return(facies)
}
