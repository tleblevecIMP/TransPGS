#' plot model transiogram against data model
plot_transio_mod<-function(model,distance,nfacies){
  layout(matrix(c(1,2,3,4,5,6,7,8,9),3,3,byrow=TRUE))
  for( i in seq(nfacies^2)){
    plot(distance,model[i,],col="blue",lwd=2,type="l",ylab="transition probability")
  }
}
