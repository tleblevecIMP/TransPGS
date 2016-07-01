#' computes the vertical transiogram matrix on a 2D section
#' @param facies is a list of matrix of 1 and 0 expressing location of each facies
#' each element of the matrix represents a location on the 2D section
#' @param vert_length is the total hight of the study in real dimensions
#' @param n_vert is the number of cells in the vertical dimension
#' @param transio_length is the length of the transiogram in real dimension
#' it should be lower than a quarter of the vert_length


vert_transio_matrix<-function(nfacies,facies,vert_length,n_vert,transio_length){
  # lets first compute the auto transition probabilities
  mat_t<-vector("list",nfacies^2)
  size = trunc(nvert*transio_length/vert_length)

  # proportions
  p<-numeric(nfacies)
  for ( i in seq(nfacies)){
    p[i]=mean(facies[i])
  }

  # distance
  distance = numeric(size)
  for ( h in seq(size)){
    distance[h]= h*vert_length/nvert
  }

  # auto and cross transition probabilities
  for ( i in seq(nfacies)){
    for ( j in seq(nfacies)){
      tij<-numeric(size)
      fi<-facies[i]
      fj<-facies[j]
      for ( h in seq(size)){
        for ( z in seq(nrow(fi)-h)){
          tij[h]<-tij+mean(fi[z,]*fj[z+h,])
        }
        tij[h] = tij[h] / (nrow(fi)-h)
      }
      mat_t[(i-1)*nfacies+j]<-tij/p[i] # we don t forget to divide by the proportion
    }
  }
  plot_transio(mat_t,distance)
}
