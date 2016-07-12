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

  p=matrix(rep(0,nfacies*size),size,nfacies-1)
  for ( i in seq(nfacies-1)){
    f<-facies[[i]]
    if (lat ==0){
      for (ligne in seq(size)){
        p[ligne,i] =  mean(f[ligne,],na.rm=TRUE)
      }
    }
    else{
      for (col in seq(size)){
        p[col,i] = mean(f[,col],na.rm=TRUE)
      }
    }
  }

  p_cum<-rep(0,size)
  for ( i in seq(nfacies-1)){
    p_cum <- p_cum +p[,i]
    lines(p_cum,seq(size)*length/size,col=i)
  }

  legend(0.8,length*0.8,images[1:(nfacies-1)],lty=rep(1,nfacies-1),col=seq(nfacies-1))
}
