#' computes the transiogram matrix on a 2D section in the lateral or vertical direction
#' each element of the matrix represents a location on the 2D section
#' @param length is the size of the study in real dimensions in the desired direction
#' @param n_pix is the number of cells in the desired direction
#' @param transio_length is the length of the transiogram in real dimension
#' it should be lower than a quarter of the vert_length


transio_matrix<-function(nfacies,facies,length,n_pix,transio_length,vertical){

  mat_t<-vector("list",(nfacies)^2)
  size = pixels_distance(n_pix,length,transio_length)
  p<-proportion_2D(facies,nfacies) #proportion will be useful for the computation of the transition prob
  distance<-distance_vector(size,length,n_pix)

  print(facies)
  # auto and cross transition probabilities computation
  for ( i in seq(nfacies)){
    for ( j in seq(nfacies)){
      tij<-numeric(size)
      fi<-binarize_facies(facies,i)
      fj<-binarize_facies(facies,j)

      if (vertical ==1 ){
        for ( h in seq(size)){
          for ( z in seq(nrow(fi)-h)){
            tij[h]<-tij[h]+mean(fi[z,]*fj[z+h,],na.rm=TRUE)
          }
          tij[h] = tij[h] / (nrow(fi)-h)
        }
      }
      if (vertical==0){
        for ( h in seq(size)){
          for ( x in seq(ncol(fi)-h)){
            tij[h]<-tij[h]+mean(fi[,x]*fj[,x+h],na.rm=TRUE)
          }
          tij[h] = tij[h] / (ncol(fi)-h)
        }
      }

      mat_t[[(i-1)*(nfacies)+j]]<-tij/p[i] # we don t forget to divide by the proportion
    }
  }

  mat_t <- do.call(rbind,mat_t)
  plot_transio(mat_t,distance,nfacies)
  mat_t<-rbind(mat_t,distance)
  return(mat_t)
}
