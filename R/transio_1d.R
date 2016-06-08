#' compute the transiograms on a vector (1D)
#' @param i is the facies number from
#' @param j is the facies number to
#' @param size is the number of cells to compute the transiogram on

library("binhf")

transio_1d<-function(i,j,section,size){
  section_size=length(section)
  pFi=sum(section==i)/section_size

  #initialization
  if(i==j){tij=rep(1,size)}
  else {tij=rep(0,size)}

  # computation
  for (h in 1:(size-1)){
    section_h=shift(section,h,"left") # shift the section of h to be able to compare the vectors afterwards
    pairs = section_size -h
    tij[h+1] = sum(section[1:pairs]==i & section_h[1:pairs] == j)/ (pairs*pFi)
  }

  return(tij)

}
