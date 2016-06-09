#' this function aims at building a perfect asymmetrical sequence with pgs
#' the parameters are found thanks to the literal expression with thresholds at 0
#' The shift is not a parameter as calculated to be perfect for asymmetry

asym_sequence_pgs<-function(size,dx,rho,r1,r2){

  shift = sqrt(-(r1^2)*log(rho))
  grid<-shifted_pgs(size,dx,r1,r2,seed,rho,0,0,shift)
  plot(grid)

}
