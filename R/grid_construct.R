# constructing 3d grid with data to be simulated

grid_construct<-function(nx,ny,data,data_location){
  grid<-array(NA,c(nrow(data),nx,ny))
  data[is.na(data)]=Inf # these values will be simulated by the gibbs sampling
  for ( i in 1:(ncol(data))){
    grid[,data_location[i,1],data_location[i,2]] <-data[,i]
  }
  return(grid)
}
