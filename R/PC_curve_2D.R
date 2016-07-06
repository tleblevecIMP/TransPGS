#' computing the proportion curves from a 2d image
#' lat is a binary defining is the user computes the lateral proportion curves
#'
#' library("bmp")
library("RGeostats")

PC_curve_2D<-function(images,length,lat){

  windows()
  nfacies = length(images)
  plot(c(0,1),c(0,length),xlab='cumulative proportion',ylab='location')
  title('proportion curve')
  facies <- reading_facies_images(images)
  if( lat ==0 ){
    size = nrow(facies[[1]])
  }
  else {size = ncol(facies[[1]]) }

  p=numeric(size)
  for ( i in seq(nfacies)){
    fi<-facies[[i]]
    if (lat ==0){
      for (ligne in seq(size)){
        # we plot the cumulative facies proportions
        p[ligne] = p[ligne]+ mean(fi[ligne,])
      }
    }
    else{
      for (col in seq(size)){
        # we plot the cumulative facies proportions
        p[col] = p[col]+ mean(fi[,col])
      }
    }
    lines(p,seq(size)*length/size,col=i)
  }
  legend(0.8,length*0.8,images,lty=rep(1,nfacies),col=seq(nfacies))
}
