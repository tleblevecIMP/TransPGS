#' computing the proportion curves from a 2d image
#'
#' library("bmp")
library("RGeostats")

PC_curve_2D<-function(images,nfacies,vert_length){

  windows()
  facies <- reading_facies_images(images)
  size = nrow(facies[[1]])
  for ( i in seq(nfacies)){
    fi<-facies[[i]]
    p=numeric(size)
    for (ligne in seq(size)){
      p[ligne] = p[ligne]+ mean(fi[ligne,]) # we plot the cumulative facies proportions
    }
    plot(p,seq(size)*vert_length/size)
  }
}
