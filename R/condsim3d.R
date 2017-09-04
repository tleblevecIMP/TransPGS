# perform the different step for a conditional simulation in three dimensions

condsim3d<-function(props,grid,r1v,r1hx,r1hy,f,r2v,r2hx,r2hy,f2,rho,shift,N,dz,dx,dy,nz,nx,ny){
  Y1<-sim3d(r1v,r1hx,r1hy,f,N,dz,dx,dy,nz,nx,ny)
  grid1 <- binarize_facies(grid,1)
  q1=qnorm(props[1])
  Y1well<-gibbs_sampling_mix(grid1,q1,dz,dx,dy,r1v,r1hx,r1hy,f,50)

  # kriging
  e1well <- Y1well - Y1
  e1<-dual_krig_surf(e1well,r1hx,r1hy,dx,dy)
  Y1c<-e1+Y1

  #second simulation
  grid2<-grid
  grid2[grid ==1]=Inf
  grid2[grid ==2]=1
  grid2[grid ==3]=0
  Y2<-sim3d(r2v,r2hx,r2hy,f2,N,dz,dx,dy,nz,nx,ny)

  # computing the thresholds according to the first simulation
  q2=threshold_fitting(props[1],props[2],rho,100)
  q2map<- grid
  q2map[grid==2] =q2map[grid==3]= q2
  q2map[grid==1]=Inf
  rho1a = exp(-(shift/r1v)^2)*cos(f*shift)
  for ( z in 1:(nz-shift/dz)){
    q2map[z,,] = (q2map[z,,]-rho*Y1c[z+shift/dz,,]/rho1a)/sqrt(1-(rho^2)/(rho1a^2)) # the shift is supposed completely vertical
  }

  # gibb sampling
  Y2well<-gibbs_sampling_map(grid2,q2map,dz,dx,dy,r2v,r2hx,r2hy,f2,50)
  Z2well<-Y2well
  for ( z in 1:(nz-shift/dz)){
    Z2well[z,,]=Y2well[z,,]*sqrt(1-(rho^2)/(rho1a^2))+Y1c[z+shift/dz,,]*(rho)/(rho1a)
  }

  #kriging
  e2well <- Y2well - Y2
  e2<-dual_krig_surf(e2well,r2hx,r2hy,nx,ny)
  Y2c<-e2+Y2
  Z2c<-Y2c
  for ( z in 1:(nz-shift/dz)){
    Z2c[z,,]=Y2c[z,,]*sqrt(1-(rho^2)/(rho1a^2))+Y1c[z+shift/dz,,]*(rho)/(rho1a)
  }

  #truncation
  gridsim<-grid
  gridsim[Y1c<q1]=1
  gridsim[Y1c>q1 & Z2c<q2]=2
  gridsim[Y1c>q1 & Z2c >q2 ]=3

  return(gridsim)
}
