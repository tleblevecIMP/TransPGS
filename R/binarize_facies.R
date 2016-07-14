#' binarize the facies matrix according to the facies wanted
#'

binarize_facies<-function(facies,i){
  facies[facies==i]<-1
  facies[facies != i]<-0
  return(facies)
}
