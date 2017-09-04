#computes a covariance matrix

cov_data<-function(data_loc,dz,dhx,dhy,rhx,rhy,rv,b){
  n=nrow(data_loc)
  dist_hx <- dist_hy <- dist_v<- matrix(0,n,n)
  for ( i in 1:(n-1)){
    for (j in (i+1):n ){
      dist_v[i,j]=dist_v[j,i]=abs(data_loc[i,1]-data_loc[j,1])*dz
      dist_hx[i,j]=dist_hx[j,i]=abs(data_loc[i,2]-data_loc[j,2])*dhx
      dist_hy[i,j]=dist_hy[j,i]=abs(data_loc[i,3]-data_loc[j,3])*dhy
    }
  }

  cov_h=exp(-sqrt( ((dist_hx/rhx)^2) + ((dist_hy/rhy)^2) ))
  cov_v =exp(-abs(dist_v/rv))*cos(b*dist_v)
  #separable model
  return(cov_h*cov_v)
}
