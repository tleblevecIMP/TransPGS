#' function to compute n simulations and plot the transiogram associated
#' it assumes one layout window is already opened
#' it is in one dimension but put two dimensions to visualize the colors
#' @param i is the facies from
#' @param j is the facies to
#' @param model1 is a Rgeostat model
#' @param model2 is a RGeostat model
#' @param xdim is the size of the xcell
#' @param distance is the vector of the distances

library("RGeostats")

nsimu_transio<-function(n,i,j,n_cell,shift_cell,a,b,rho,rho1a,model1,model2,xdim, distance){
  neigh=new("neigh",ndim=2,type=0);
  tijmean=rep(0,length(distance))
  for ( seed in 1:n){
    # simulation
    grid<-db.create(flag.grid=TRUE,x0=c(0,0),nx=c(n_cell+shift_cell,1),dx=c(xdim,1))
    grid<-simtub(dbout=grid,model=model1,neigh=neigh,seed=seed+400,nbsimu=2)

    facies<- shifted_trunc(grid[,4],grid[,5],rho,a,b,rho1a,shift_cell)
    grid<-db.add(grid,facies)

    # computing transio
    tij<-transio_1d(i,j,facies,length(distance))
    tijmean = tijmean + tij
    lines(distance,tij)
  }
  lines(distance,tijmean/n,col="red",lwd=2)
}
