#' function that plots transiograms
#' the function windows() must be called before the call of this function
#' @param transios is the list of the different transiograms to plot
#'

plot_transio<-function(transios,distance){


  layout(matrix(seq(length(transios)),2,2,byrow=TRUE))

  for ( i in seq(nrow(transios))){
    plot(distance,transios[i,],col="blue",type="l",ylim=c(0,1),ylab='transition probability',lwd=2)
  }
}
