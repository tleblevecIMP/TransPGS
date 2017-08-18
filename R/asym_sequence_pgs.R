#' this function aims at building a perfect asymmetrical sequence with pgs
#' the parameters are found thanks to the literal expression with thresholds at 0
#' The shift is not a parameter as calculated to be perfect for asymmetry

asym_sequence_pgs<-function(nx,dx,rho,r1,r2){

  windows()
  shift=0
  if (rho!=0) {shift = sqrt(-(r1^2)*log(rho))}
  grid<-shifted_pgs1d(nx,dx,r1,r2,seed,rho,0,0,shift)
  grid<-matrix(grid,length(grid),1)
  image(grid)

}
