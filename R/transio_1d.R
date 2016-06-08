#' compute the transiograms on a vector (1D)
#' @param i is the facies number from
#' @param j is the facies number to

library("binhf")

transio_1d<-function(i,j,section,max_dist){
  pFi=sum(section[section==i])/length(section)

  #initialization
  tij=rep(0,max_dist+1)
  if(i==j){tij[1]=1}
  else {tij[1]=0}

  for (h in 1:max_dist){
    section_h=shift(section,h,"right") # shift the section of h to be able to compare the vectors afterwards
    tij[h+1] = sum(section[section[rep(1:(max_dist-h))]==i & section_shift[rep(h:max_dist)] == j])/ ((max_dist-h)*pFi)
  }

  return(tij)

}
