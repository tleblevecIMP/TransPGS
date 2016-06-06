#' finding the thresholds for a truncation rule with 3 facies
#' matching with the proportions with the multigaussian integral of mvtnorm
#' @param pF1 is the proportion of facies 1
#' @param pF2 is the proportion of facies 2
#' @param rho is the correlation between the two gaussian functions
#' @param iterations is the number of iterations to match with the proportions
#'
#' @return  t2 is the trunction for the second gaussian function

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
    }
  }
  return(b_fit)
}
