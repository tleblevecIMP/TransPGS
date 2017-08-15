#' plot model transiogram against data model
plot_transio_data_mod<-function(model,distance,data,nfacies){
  layout(matrix(c(1,2,3,4,5,6,7,8,9),3,3,byrow=TRUE))
  for( i in seq(nfacies^2)){
    plot(data[10,],data[i,],col="black",type="p",ylim=c(0,1),ylab='transition probability',lwd=2)
    lines(distance,model[i,],col="blue",lwd=2)
  }
}
