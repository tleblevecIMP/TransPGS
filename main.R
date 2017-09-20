

# test one one log of the latemar
facies<-reading_facies_logs("latemar_section.txt",5)
props<-proportion_2D(facies,5)
t<-transio_analysis_log("latemar_section.txt",5,10,5)
v<-vario_analysis_log("latemar_section.txt",5,10,5)
#computing some parameters of interest
r13h= t[3,]/(1-t[1,])
r13mh=props[3]*t[11,]/((1-t[1,])*props[1])
r31h=t[11,]/(1-t[13,])
v13h=v[3,]/v[1,]
v31h=v[11,]/v[13,]
beucherh=0.5*(props[1]*t[3,]+props[3]*t[11,])/(1-props[1]*t[1,])
n=length(t[3,])
x = (1:n)*5/n
layout(matrix(seq(1),1,1,byrow=TRUE))
plot(x,r13h,type="l",ylim=c(0,1))
lines(x,r13mh,col="orange")
lines(x,v13h,col="red")
lines(x,t[3,]*props[1],col="blue")
lines(x,t[11,]*props[3],col="green")
legend(0.,1,c("-v13(h)/v11(h)","t13(h)/(1-t11(h))","t13(-h)/(1-t11(-h))","S2(h)","S2(-h)"), # puts text in the legend

       lty=c(1,1,1,1,1), # gives the legend appropriate symbols (lines)

       lwd=c(2.5,2.5,2.5,2.5,2.5),col=c("red","black","orange","blue","green")) # gives the legend lines the correct color and width)

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
