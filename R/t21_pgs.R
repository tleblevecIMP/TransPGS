# Function to compute and plot the transiograms from gaussian parameters
# using the mvtnorm package for the multivariate gaussian integral



t21_pgs<-function(nb_facies,props,r1,rho,shift,distance,step){

  pF1=props[1]
  pF2=props[2]
  pF3 = 1- pF1- pF2
  length=trunc(distance/step)
  x=numeric(length)
  t21<-numeric(length)

  a = qnorm(pF1)
  b<-threshold_fitting(pF1,pF2,rho,100.)

  for (h in seq(step,distance,step)){
    rho1h=exp(-(h^2)/(r1^2))
    rho1a = exp(-(shift^2)/(r1^2))
    rho21h =  rho*exp(-(abs(h-shift)^2)/(r1^2)) / rho1a
    sigma <- matrix(c(1,rho,rho1h,rho,1,rho21h,rho1h,rho21h,1),3,3)
    t21[h]<- pmvnorm(lower=c(a,b,-Inf),upper=c(Inf,Inf,a),mean = c(0,0,0),sigma=sigma)/pF2
    x[h]<-h
  }
  plot(x,t21)
}
