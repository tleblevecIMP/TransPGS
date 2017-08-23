#' function to compute n simulations and plot the transiogram associated
#' it assumes one layout window is already opened
#' it is in one dimension but put two dimensions to visualize the colors
#' @param i is the facies from
#' @param j is the facies to
#' @param model1 is a Rgeostat model
#' @param model2 is a RGeostat model
#' @param xdim is the size of the xcell
#' @param distance is the vector of the distances


nsimu_transio<-function(n,i,j,n_cell,shift,a,b,rho,r1,r2,dx, distance){
  tijmean=rep(0,length(distance))
  for ( seed in 1:n){

    # simulation
    grid<-shifted_pgs(n_cell,dx,r1,r2,seed,rho,a,b,shift)

    # computing transio
    tij<-transio_1d(i,j,grid[,6],length(distance))
    tijmean = tijmean + tij
    lines(distance,tij)
  }
  lines(distance,tijmean/n,col="red",lwd=2)
}
