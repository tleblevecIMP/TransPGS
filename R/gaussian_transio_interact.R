#' compute the transiograms from the gaussian coregionalization model
#' @param dist_vert is the vertical distance the transiogram displays
#' @param dist_hor is the horizontal distance the transiogram displays

library(tcltk2)
library("RGeostats")

gaussian_transio_interact<-function(props,dist_vert,dist_hor,dx,dy,nx,ny){
  windows()
  pF1=props[1]
  pF2 = props[2]
  pF3=1-pF1-pF2

  # this values are just for the initialization
  a= qnorm(pF1)
  rho = 0
  b = threshold_fitting(pF1,pF2,rho,20)
  shift=0
  r1<-0.6
  r2<-0.2
  rho1a <- gaussian_cov(r1,shift)

  #sliders
  win1 <- tktoplevel()
  tktitle(win1) <- "Infering of the model"
  sliderValuerho <- tclVar("0")
  sliderValuerange1 <- tclVar("0.5")
  sliderValuean1 <- tclVar("1.")
  sliderValuerange2 <- tclVar("0.2")
  sliderValuean2 <- tclVar("1.")
  sliderValueshift <- tclVar("0.")
  win1$env$labelrho <- tk2label(win1,
                                text = "rho: 0")
  win1$env$labelrange1 <- tk2label(win1,
                                   text = "range of Y1: 0.5")
  win1$env$labelan1 <- tk2label(win1,
                                   text = "anisotropy of Y1: 0.5")
  win1$env$labelrange2 <- tk2label(win1,
                                   text = "range of Y2: 0.2")
  win1$env$labelan2 <- tk2label(win1,
                                   text = "anisotropy of Y2: 0.5")
  win1$env$labelshift <- tk2label(win1,
                                  text = "shift: 0.")

  onChange <- function(...) {
    #sliders
    valuerho <- floor(as.double(tclvalue(sliderValuerho))*1000)/1000.
    valuerange1 <- floor(as.double(tclvalue(sliderValuerange1))*100)/100.
    valuean1 <- floor(as.double(tclvalue(sliderValuean1))*10)/10.
    valuerange2 <- floor(as.double(tclvalue(sliderValuerange2))*100)/100.
    valuean2 <- floor(as.double(tclvalue(sliderValuean2))*100)/100.
    valueshift <- floor(as.double(tclvalue(sliderValueshift))*100)/100.
    # the value parameters are going to be changed by another function
    newrho <- valuerho
    r1<-valuerange1
    an1<-valuean1
    r2<-valuerange2
    an2<-valuean2
    shift<-valueshift
    labelrho <- sprintf("rho: %s", valuerho)
    labelrange1 <- sprintf("range of Y1: %s", valuerange1)
    labelan1 <- sprintf("anisotropy of Y1: %s", valuean1)
    labelrange2 <- sprintf("range of Y2: %s", valuerange2)
    labelan2 <- sprintf("anisotropy of Y2: %s", valuean2)
    labelshift <- sprintf("shift: %s", valueshift)
    tkconfigure(win1$env$labelrho, text = labelrho)
    tkconfigure(win1$env$labelrange1, text = labelrange1)
    tkconfigure(win1$env$labelan1, text = labelan1)
    tkconfigure(win1$env$labelrange2, text = labelrange2)
    tkconfigure(win1$env$labelan2, text = labelan2)
    tkconfigure(win1$env$labelshift, text = labelshift)

    # b is the only parameter that is not defined by the user
    # is different when rho changes
    if ( newrho != rho){
      rho = newrho
      b<-threshold_fitting(pF1,pF2,rho,20)
    }

    result_vert<-transio_pgs(props,r1,r2,rho,a,b,shift,dist_vert,dx)
    result_hor<-transio_pgs(props,r1*an1,r2*an2,rho,a,b,shift,dist_hor,dy)
    rho1a=gaussian_cov(r1,shift)

    if (rho<=rho1a){
      #plot_transio_data_mod(result_vert[1:9,],result_vert[10,],data_vert,3)
      layout(matrix(c(1,2,3,4,5,6,7,8,9),3,3,byrow=TRUE))
      for( i in seq(9)){
        plot(result_vert[10,],result_vert[i,],col="black",type="l",ylim=c(0,1),ylab='transition probability',lwd=2)
      }
      #dev.set(dev.next())
      #layout(matrix(c(1,2,3,4,5,6,7,8,9),3,3,byrow=TRUE))
      #for( i in seq(9)){
      #  plot(result_hor[10,],result_hor[i,],col="black",type="l",ylim=c(0,1),ylab='transition probability',lwd=2)
      #}
      #plot_transio_data_mod(result_hor[1:9,],result_hor[10,],data_hor,3)
      #dev.set(dev.next())
    }
  }

  seed =0
  simulation <- function(...){
    #sliders
    valuerho <- floor(as.double(tclvalue(sliderValuerho))*1000)/1000.
    valuerange1 <- floor(as.double(tclvalue(sliderValuerange1))*100)/100.
    valuean1 <- floor(as.double(tclvalue(sliderValuean1))*10)/10.
    valuerange2 <- floor(as.double(tclvalue(sliderValuerange2))*100)/100.
    valuean2 <- floor(as.double(tclvalue(sliderValuean2))*100)/100.
    valueshift <- floor(as.double(tclvalue(sliderValueshift))*100)/100.
    # the value parameters are going to be changed by another function
    newrho <- valuerho
    r1<-valuerange1
    an1<-valuean1
    r2<-valuerange2
    an2<-valuean2
    shift<-valueshift
    labelrho <- sprintf("rho: %s", valuerho)
    labelrange1 <- sprintf("range of Y1: %s", valuerange1)
    labelan1 <- sprintf("anisotropy of Y1: %s", valuean1)
    labelrange2 <- sprintf("range of Y2: %s", valuerange2)
    labelan2 <- sprintf("anisotropy of Y2: %s", valuean2)
    labelshift <- sprintf("shift: %s", valueshift)
    tkconfigure(win1$env$labelrho, text = labelrho)
    tkconfigure(win1$env$labelrange1, text = labelrange1)
    tkconfigure(win1$env$labelan1, text = labelan1)
    tkconfigure(win1$env$labelrange2, text = labelrange2)
    tkconfigure(win1$env$labelan2, text = labelan2)
    tkconfigure(win1$env$labelshift, text = labelshift)

    seed=seed+1
    b<-threshold_fitting(pF1,pF2,rho,20)
    grid<-shifted_pgs(nx,ny,dx,dy,r1,an1,r2,an2,seed,newrho,a,b,0)
    plot(grid)
  }

  tkgrid(win1$env$labelrho, padx = 0.1, pady = c(10, 5))
  win1$env$sliderrho <- tk2scale(win1, from = -0.99, to = 0.99,
                                 variable = sliderValuerho, orient = "horizontal", length = 500,
                                 command = onChange)
  tkgrid(win1$env$sliderrho, padx = 0.1, pady = c(5, 10))

  tkgrid(win1$env$labelrange1, padx = 0.1, pady = c(10, 5))
  win1$env$sliderrange1 <- tk2scale(win1, from = 0.01, to = 2,
                                    variable = sliderValuerange1, orient = "horizontal", length = 500,
                                    command = onChange)
  tkgrid(win1$env$sliderrange1, padx = 0.1, pady = c(5, 10))

  tkgrid(win1$env$labelan1, padx = 0.1, pady = c(10, 5))
  win1$env$slideran1 <- tk2scale(win1, from = 0.1, to = 10,
                                    variable = sliderValuean1, orient = "horizontal", length = 500,
                                    command = onChange)
  tkgrid(win1$env$slideran1, padx = 0.1, pady = c(5, 10))


  tkgrid(win1$env$labelrange2, padx = 0.1, pady = c(10, 5))
  win1$env$sliderrange2 <- tk2scale(win1, from = 0.01, to = 2,
                                    variable = sliderValuerange2, orient = "horizontal", length = 500,
                                    command = onChange)
  tkgrid(win1$env$sliderrange2, padx = 0.1, pady = c(5, 10))

  tkgrid(win1$env$labelan2, padx = 0.1, pady = c(10, 5))
  win1$env$slideran2 <- tk2scale(win1, from = 0.1, to = 10,
                                    variable = sliderValuean2, orient = "horizontal", length = 500,
                                    command = onChange)
  tkgrid(win1$env$slideran2, padx = 0.1, pady = c(5, 10))


  tkgrid(win1$env$labelshift, padx = 0.1, pady = c(10, 5))
  win1$env$slidershift <- tk2scale(win1, from = -2, to = 2,
                                   variable = sliderValueshift, orient = "horizontal", length = 500,
                                   command = onChange)
  tkgrid(win1$env$slidershift, padx = 0.1, pady = c(5, 10))

  win1$env$butOK <-tk2button(win1, text = "OK", width = -6, command = simulation)
  tkgrid(win1$env$butOK, padx = 10, pady = c(5, 10))

}












