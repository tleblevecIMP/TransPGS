#' function that plots transiograms
#' @param transios is the list of the different transiograms to plot
#' 

plot_transio<-function(transios,distance){
  
  windows()
  layout(matrix(seq(length(transios)),2,2,byrow=TRUE))
  
  for ( i in seq(length(transios))){
    plot(distance,transios[i,],col="blue",type="l",ylim=c(0,1),ylab='transition probability')
  }
}