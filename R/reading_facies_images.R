#' reading images with facies
#' the files must be .bmp
#'

library("bmp")

reading_facies_images<-function(images){

  facies <-vector("list",length(images))
  for ( i in 1:length(images)){
    f<-read.bmp(images[[i]])
    f[f != 0]<-1
    facies[[i]]<-f
  }

  return(facies)
}
