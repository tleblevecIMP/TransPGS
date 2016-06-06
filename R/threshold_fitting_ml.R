#' threshold fitting with a maximul likelood algorithm
#' it avoids to compute the gaussian integral
#' but requires to do random simulations with the same package
#' test should be done to see which method is the quickest
#'
#'The proportion does not converge for now, BUG
#'
#'#' @param pF1 is the proportion of facies 1
#' @param pF2 is the proportion of facies 2
#' @param rho is the correlation between the two gaussian functions
#' @param iterations is the number of iterations to match with the proportions
#' @return  t2 is the trunction for the second gaussian function

threshold_fitting_ml<-function(pF1,pF2,rho,iterations){

  t1 = qnorm(pF1)
  # random simulation with correlation rho
  gauss<-rmvnorm(iterations,c(0,0),matrix(c(1,rho,rho,1),2,2))
  logml_max=-Inf
  t2_fit=0
  for(inc1 in 1:(iterations-1)){
    t2 = qnorm(inc1 /iterations)
    x= gauss[,1]>t1 & gauss[,2]>t2
    # maximum likelihood for a bernoulli distribution
    logml = sum(x) *log(pF2) + (iterations-sum(x))*log(1-pF2)
    if (logml > logml_max){
        logml_max = logml
        t2_fit = t2
    }
  }
  return(t2_fit)
}
