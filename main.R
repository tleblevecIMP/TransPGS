

# test one one log of the latemar
facies<-reading_facies_logs("latemar_section.txt",5)
props<-proportion_2D(facies,5)
t<-transio_analysis_log("latemar_section.txt",5,10,3)

# other test
#infering model
facies<-reading_facies_logs("latemar60m.csv",3)
props<-proportion_2D(facies,5)
transio<-transio_analysis_log("latemar60m.csv",3,10,1)
gaussian_transio_interact(props,transio,1,1,100,100)

# simulation in three dimensions

# parameters of the simulation
nx=10
ny=10
nz=nrow(facies)
dz=0.1
dx=dy=1
N=100
f=f2=0
r1v=r2v=1
r1hx=r1hy=r2hx=r2hy=10
rho=0
shift=0
nwell=ncol(facies)

# preparing the grid
# the vector is the vector of locations of the different wells (supposed all vertical)
data_location<-matrix(c(1,1,5,10),nwell,2,byrow = TRUE)
grid<-grid_construct(nx,ny,facies,data_location)

#sim
sim<-condsim3d(props,grid,r1v,r1hx,r1hy,f,r2v,r2hx,r2hy,f2,rho,shift,N,dz,dx,dy,nz,nx,ny)


# test on Martin paper

rf1<-reading_facies_logs("test2.txt",3)
prps<-proportion_2D(rf1,3)
transio<-transio_analysis_log("test2.txt",3,141,50)
library(mvtnorm)
gaussian_transio_interact(prps,transio,1,1,100,100)
facies<-rf1
nx=6
ny=4
nz=nrow(facies)
dz=1
dx=100
dy=100
N=10
f=f2=0
r1v=0.5
r2v=3.43
r1hx=r1hy=2.7*0.5
r2hx=r2hy=3.43*1.74
rho=0.48
shift=0
nwell=ncol(facies)
facies<-rf1
data_location<-matrix(c(3,1,4,1,5,1,6,1,1,2,2,1,6,4),nwell,2,byrow = TRUE)
grid<-grid_construct(nx,ny,rf1,data_location)
sim<-condsim3d(prps,grid,r1v,r1hx,r1hy,f,r2v,r2hx,r2hy,f2,rho,shift,N,dz,dx,dy,nz,nx,ny)
