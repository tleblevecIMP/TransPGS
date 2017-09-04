# constructing 3d grid with data to be simulated

grid_construct<-function(nx,ny,data,data_location){
  grid<-array(NA,c(nrow(facies),nx,ny))
  data[is.na(data)]=Inf # these values will be simulated by the gibbs sampling
  for ( i in ncol(data)){
    grid[,data_loc[i,1],data_loc[i,2]] <-data[,i]
  }
  return(grid)
}
