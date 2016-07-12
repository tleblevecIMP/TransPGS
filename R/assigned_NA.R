#' assigned non values on images when the indicator is never 1
#' @param facies is a list of indicator matrix representing each facies

assigned_NA<-function(facies){
  for ( x in seq(nrow(facies[[1]]))){
    for ( y in seq(ncol(facies[[1]]))){
      sum =0
      for (f in facies){
        sum =sum + f[x,y]
      }
      if ( sum == 0){
        for(i in seq(length(facies))){
          facies[[i]][x,y]=NA
        }
      }
    }
  }
  return(facies)
}
