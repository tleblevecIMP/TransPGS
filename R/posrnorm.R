# truncated gaussian variable

negrnorm<-function(q,mean,sd){
  a=rnorm(1,mean,sd)
  while(a>q){
    a=rnorm(1,mean,sd)
  }
  return(a)
}

posrnorm<-function(q,mean,sd){
  a=rnorm(1,mean,sd)
  while(a<q){
    a=rnorm(1,mean,sd)
  }
  return(a)
}

truncnorm<-function(qmin,qmax,mean,sd){
  a=rnorm(1,mean,sd)
  while(a<qmin | a >qmax){
    a=rnorm(1,mean,sd)
  }
  return(a)
}
