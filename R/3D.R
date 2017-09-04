# Perform a conditional 3d simulation

#unconditional simulation
sim3d<-function(rv,rhx,rhy,freq,N,dz,dx,dy,nz,nx,ny){
  phase <- runif(N,0,2*pi)
  bin <- sample(c(-1,1),N,replace=T)
  w1 <- rnorm(N, mean = freq , sd = sqrt(2)/rv)
  w2 <- rnorm(N, mean = 0 , sd =1/rhx)
  w3 <- rnorm(N, mean = 0 , sd =1/rhy)
  sim<-array(0,c(nz,nx,ny))
  for(i in 1:N){
    for ( z in 1:nz){
      for ( x in 1:nx){
        for ( y in 1:ny){
          sim[z,x,y] = sim[z,x,y] + cos(bin[i]*w1[i]*(z*dz)+w2[i]*(x*dx)+w3[i]*(y*dy)+phase[i])
        }
      }
    }
  }
  sim = sim *sqrt(2/N)

  return(sim)
}


#local conditional simulation at data location
gibbs_sampling_mix<-function(grid,q,dz,dhx,dhy,rv,rhx,rhy,b,it){
  # this algorithm is based on Emery paper (2013); algorithm 7

  data_values<-grid[!is.na(grid)] # getting the values
  data_loc<-which(!is.na(grid),arr.ind=TRUE) # getting the position in the grid
  n=length(data_values)
  neg <-which(data_values==1)
  pos <- which(data_values==0)
  und <- which(data_values==Inf)

  # computation of the covariance matrix
  c_data<- cov_data(data_loc,dz,dhx,dhy,rhx,rhy,rv,b)
  inv <- which(c_data <0)
  #initialization uncorrelated gaussian simulation inside the interval
  negfirst = negrnorm(q,0,1)
  posfirst = posrnorm(q,0,1)
  undsim = rnorm(1,0,1)
  sim<-rep(0,n)
  for ( i in 1:n){
    if (data_values[i]==1){
      sim[i] = negfirst
    }
    if(data_values[i]==0){ sim[i] = posfirst}
    if ( data_values[i]==Inf){sim[i]=undsim}
  }

  # gibbs sampling
  for( t in 1:it){
    for ( i in 1:n){
      condi<-(((q-sim)/c_data[i,])+sim[i])
      maxi=Inf
      mini=-Inf
      for ( j in 1:n){
        # the covariance can be negative and so can change the sign of the inequality
        if (c_data[i,j]>0){
          if(data_values[j]==1){
            maxi = min(maxi,condi[j])
          }
          if(data_values[j]==0){
            mini=max(mini,condi[j])

          }
        }
        if(c_data[i,j]<0) {
          if(data_values[j]==1){
            mini= max(mini,condi[j])

          }
          if(data_values[j]==0) {
            maxi=min(maxi,condi[j])

          }
        }
      }
      V = truncnorm(mini,maxi,0,1)
      sim = sim + c_data[i,]*(V-sim[i])
    }
  }

  for ( i in 1:n){
    z<-data_loc[i,1]
    x<-data_loc[i,2]
    y<-data_loc[i,3]
    grid[z,x,y]<-sim[i]
  }
  return(grid)
}

#local conditional simulation at data location with varying proportions
gibbs_sampling_map<-function(grid,qmap,dz,dhx,dhy,rv,rhx,rhy,b,it){
  # this algorithm is based on Emery paper (2013); algorithm 7

  data_values<-grid[!is.na(grid)] # getting the values
  q<-qmap[!is.na(qmap)]
  data_loc<-which(!is.na(grid),arr.ind=TRUE) # getting the position in the grid
  n=length(data_values)
  neg <-which(data_values==1)
  pos <- which(data_values==0)
  und <- which(data_values==Inf)
  # computation of the covariance matrix
  c_data<- cov_data(data_loc,dz,dhx,dhy,rhx,rhy,rv,b)
  inv <- which(c_data <0)
  #initialization uncorrelated gaussian simulation inside the interval
  undsim = rnorm(1,0,1)
  sim<-rep(0,n)
  for ( i in 1:n){
    if (data_values[i]==1){
      sim[i] = negrnorm(q[i],0,1)
    }
    if (data_values[i]==0){ sim[i] = posrnorm(q[i],0,1)}
    if (data_values[i]==Inf){sim[i]=undsim}
  }

  # gibbs sampling
  for( t in 1:it){
    for ( i in 1:n){
      condi<-(((q-sim)/c_data[i,])+sim[i])
      maxi=Inf
      mini=-Inf
      for ( j in 1:n){
        # the covariance can be negative and so can change the sign of the inequality
        if (c_data[i,j]>0){
          if(data_values[j]==1){
            maxi = min(maxi,condi[j])
          }
          if(data_values[j]==0){
            mini=max(mini,condi[j])

          }
        }
        if(c_data[i,j]<0) {
          if(data_values[j]==1){
            mini= max(mini,condi[j])

          }
          if(data_values[j]==0) {
            maxi=min(maxi,condi[j])

          }
        }
      }
      V = truncnorm(mini,maxi,0,1)
      sim = sim + c_data[i,]*(V-sim[i])
    }
  }
  for ( i in 1:n){
    z<-data_loc[i,1]
    x<-data_loc[i,2]
    y<-data_loc[i,3]
    grid[z,x,y]<-sim[i]
  }
  return(grid)
}

#kriging of the error between unconditional and local conditional simulation
#using the screening properties of gaussian covariances
dual_krig_surf<-function(errorwell,rhx,rhy,dhx,dhy){

  # getting the positions in the grid of one horizontal line, the positions will be the same for every line
  data_loc<-which(!is.na(errorwell[1,,]),arr.ind=TRUE)
  toestim <-which(is.na(errorwell[1,,]),arr.ind=TRUE)
  n= nrow(data_loc)
  m = nrow(toestim)
  print(n)
  # covariance and inverse covariance matrix of a single line
  c_data<- cov_data_surf(data_loc,dhx,dhy,rhx,rhy)
  invc<-chol2inv(chol(c_data))
  errorfield <-errorwell
  print(c_data)
  print(invc)

  # covariance vector between estimation and data of everyline
  c_estim_data<-matrix(0,m,n)
  for ( j in 1:m){
    hx=(data_loc[,1]-toestim[j,1])*dhx
    hy=(data_loc[,2]-toestim[j,2])*dhy
    c_estim_data[j,]<-exp(-sqrt( ((hx/rhx)^2)+ ((hy/rhy)^2) ))
  }

  # for every surface
  for ( i in 1:nrow(errorwell)){
    surf<-errorwell[i,,]
    data_values<-surf[!is.na(surf)]
    b <- t(data_values) %*% invc
    for ( j in 1:m){
      hx = toestim[j,1]
      hy = toestim[j,2]
      errorfield[i,hx,hy] = b %*% c_estim_data[j,]
    }
  }


  return(errorfield)
}
