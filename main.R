

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
data_location<-matrix(c(1,1,5,10),2,nwell)
grid<-grid_construct(nx,ny,facies,data_location)

#sim
sim<-condsim3d(props,grid,r1v,r1hx,r1hy,f,r2v,r2hx,r2hy,f2,rho,shift,N,dz,dx,dy,nz,nx,ny)
