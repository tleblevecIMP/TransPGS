#' reading images with facies
#' the files must be .bmp
#'

library("bmp")

reading_facies_images<-function(images){

  facies <-vector("list",length(images))
  for ( i in 1:length(images)){
    f<-read.bmp(system.file("extdata", images[i], package = "TransPGS"))
    f[f != 0]<-1
    facies[[i]]<-f
  }
  facies <- assigned_NA(facies)

  return(facies)
}
