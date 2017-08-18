#' simulates gaussian fields with rgeostat on a grid with gaussian variogram
#'

gaussian_field<-function(nx,ny,dx,dy,r,an,seed){
  N=100
  phase <- runif(N,0,2*pi)
  bin <- sample(c(-1,1),N,replace=T)
  w1 <- rnorm(N, mean = 0 , sd = sqrt(2)/r)
  w2 <- rnorm(N, mean = 0 , sd =sqrt(2)/(r*an))
  sim<-matrix(0,nx,ny)
  for(i in 1:N){
      for ( x in 1:nx){
        for ( y in 1:ny){
          sim[x,y] = sim[x,y] + cos(w1[i]*(x*dx)+w2[i]*(y*dy)+phase[i])
      }
    }
  }
  sim = sim *sqrt(2/N)

  return(sim)
}
