#' assigned non values on images when the indicator is never 1
#' @param facies is a list of indicator matrix representing each facies

assigned_NA<-function(facies){
  facies[facies == 0]<- NA
  return(facies)
}
