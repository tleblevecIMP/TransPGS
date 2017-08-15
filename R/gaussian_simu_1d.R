grid<-db.create(flag.grid=TRUE,nx=1000,dx=0.001)
model<-model.create(ndim=1,nvar=1,vartype=4,range=0.1)
grid<-simtub(dbout=grid,model=model,seed=4,nbsimu=1)
plot(grid)
