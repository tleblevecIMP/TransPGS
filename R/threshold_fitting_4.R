#' threshold fitting for 4 facies
#'

threshold_fitting_4<-function(pF1,pF2,pF3,rho,iterations){

  Obj_min = 1000
  Sigma <- matrix(c(1,rho,rho,1),2,2)
  for (inc1 in 1:(iterations-1)){
    for(inc2 in 1:(iterations-1)){
      t1 = qnorm(inc1 /iterations)
      t2 = qnorm(inc2 /iterations)
      pF1exp = pmvnorm(lower=c(-Inf,t2),upper=c(t1,+Inf),mean=c(0,0),sigma=Sigma)
      pF2exp = pmvnorm(lower=c(t1,t2),upper=c(+Inf,+Inf),mean=c(0,0),sigma=Sigma)
      pF3exp = pmvnorm(lower=c(t1,-Inf),upper=c(+Inf,t2),mean=c(0,0),sigma=Sigma)

      Obj= abs(pF2 -pF2exp)+abs(pF1 -pF1exp)+abs(pF3 -pF3exp)
      if(Obj<Obj_min){
        t1_opt=t1
        t2_opt=t2
        Obj_min=Obj
      }
    }
  }
  return(c(t1_opt,t2_opt))
}
