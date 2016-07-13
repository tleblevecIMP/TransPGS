#' computes the vertical transiogram matrix on a 2D section
#' @param facies is a list of matrix of 1 and 0 expressing location of each facies
#' each element of the matrix represents a location on the 2D section
#' @param vert_length is the total hight of the study in real dimensions
#' @param n_vert is the number of cells in the vertical dimension
#' @param transio_length is the length of the transiogram in real dimension
#' it should be lower than a quarter of the vert_length


vert_transio_matrix<-function(nfacies,facies,vert_length,n_vert,transio_length){

  mat_t<-vector("list",(nfacies)^2)
  size = pixels_distance(n_vert,vert_length,transio_length)
  p<-proportion_2D(facies,nfacies) #proportion will be useful for the computation of the transition prob
  distance<-distance_vector(size,vert_length,n_vert)

  # auto and cross transition probabilities computation
  for ( i in seq(nfacies)){
    for ( j in seq(nfacies)){
      tij<-numeric(size)
      fi<-facies[[i]]
      fj<-facies[[j]]
      for ( h in seq(size)){
        for ( z in seq(nrow(fi)-h)){
          tij[h]<-tij[h]+mean(fi[z,]*fj[z+h,],na.rm=TRUE)
        }
        tij[h] = tij[h] / (nrow(fi)-h)
      }
      mat_t[[(i-1)*(nfacies)+j]]<-tij/p[i] # we don t forget to divide by the proportion
    }
  }

  mat_t <- do.call(rbind,mat_t)
  plot_transio(mat_t,distance,nfacies)
  return(mat_t)
}
