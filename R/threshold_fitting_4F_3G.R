#' threshold fitting for 4 facies with three gaussian functions
#'

threshold_fitting_4F_3G<-function(pF1,pF2,pF3,cor,iterations){

  Obj_min1 =Obj_min2=Obj_min3 =1000
  t1_opt= t2_opt =t3_opt = 0
  Sigma2 = matrix(c(1,cor[1],cor[1],1),2,2)
  Sigma3 <- matrix(c(1,cor[1],cor[3],cor[1],1,cor[2],cor[3],cor[2],1),3,3)

  t1_opt = qnorm(pF1)

  for (inc2 in 1:(iterations-1)){
    t2 = qnorm(inc2 /iterations)
    pF2exp = pmvnorm(lower=c(t1_opt,-Inf),upper=c(+Inf,t2),mean=c(0,0),sigma=Sigma2)
    Obj2= abs(pF2 -pF2exp)
    if(Obj2<Obj_min2){
      t2_opt=t2
      Obj_min2=Obj2
    }
  }

  for (inc3 in 1:(iterations-1)){
    t3 = qnorm(inc3 /iterations)
    pF3exp = pmvnorm(lower=c(t1_opt,t2_opt,-Inf),upper=c(+Inf,+Inf,t3),mean=c(0,0,0),sigma=Sigma3)
    Obj3= abs(pF3 -pF3exp)
    if(Obj3<Obj_min3){
      t3_opt=t3
      Obj_min3=Obj3
    }
  }
  return(c(t1_opt,t2_opt,t3_opt,Obj_min1,Obj_min2,Obj_min3))
}
