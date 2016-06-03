# finding the thresholds for a truncation rule with 3 facies
# matching with the proportions with the multigaussian integral of mvtnorm

threshold_fitting<-function(pF1,pF2,rho,iterations){

  t1 = qnorm(pF1)
  Obj_min = 1000
  for (inc1 in 1:(iterations-1)){
    t2 = qnorm(inc1 /iterations)
    Sigma <- matrix(c(1,rho,rho,1),2,2)
    pF2exp = pmvnorm(lower=c(t1,t2),upper=c(+Inf,+Inf),mean=c(0,0),sigma=Sigma)
    Obj= abs(pF2 -pF2exp)
    if(Obj<Obj_min){
      Obj_min=Obj
      b_fit = t2
      pF2_fit = pF2exp
      pF3_fit= 1-pF1-pF2_fit
    }
  }
  t2=b_fit
  return(t2)
}
