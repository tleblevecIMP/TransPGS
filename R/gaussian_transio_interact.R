#' compute the transiograms from the gaussian coregionalization model
#' @param dist is the distance at which we want to compute transiograms

library(tcltk2)
library("RGeostats")

gaussian_transio_interact<-function(props,dist,dx,data){
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

  windows() # windows on which the plots will be done

  #sliders
  win1 <- tktoplevel()
  tktitle(win1) <- "Infering of the model"
  sliderValuerho <- tclVar("0")
  sliderValuerange1 <- tclVar("0.5")
  sliderValuerange2 <- tclVar("0.2")
  sliderValueshift <- tclVar("0.")
  win1$env$labelrho <- tk2label(win1,
                                text = "rho: 0")
  win1$env$labelrange1 <- tk2label(win1,
                                   text = "range of Y1: 0.5")
  win1$env$labelrange2 <- tk2label(win1,
                                   text = "range of Y2: 0.2")
  win1$env$labelshift <- tk2label(win1,
                                  text = "shift: 0.")

  onChange <- function(...) {
    #sliders
    valuerho <- floor(as.double(tclvalue(sliderValuerho))*1000)/1000.
    valuerange1 <- floor(as.double(tclvalue(sliderValuerange1))*100)/100.
    valuerange2 <- floor(as.double(tclvalue(sliderValuerange2))*100)/100.
    valueshift <- floor(as.double(tclvalue(sliderValueshift))*100)/100.
    # the value parameters are going to be changed by another function
    newrho <- valuerho
    r1<-valuerange1
    r2<-valuerange2
    shift<-valueshift
    labelrho <- sprintf("rho: %s", valuerho)
    labelrange1 <- sprintf("range of Y1: %s", valuerange1)
    labelrange2 <- sprintf("range of Y2: %s", valuerange2)
    labelshift <- sprintf("shift: %s", valueshift)
    tkconfigure(win1$env$labelrho, text = labelrho)
    tkconfigure(win1$env$labelrange1, text = labelrange1)
    tkconfigure(win1$env$labelrange2, text = labelrange2)
    tkconfigure(win1$env$labelshift, text = labelshift)

    # b is the only parameter that is not defined by the user
    # is different when rho changes
    if ( newrho != rho){
      rho = newrho
      b<-threshold_fitting(pF1,pF2,rho,20)
    }

    result<-transio_pgs(props,r1,r2,rho,a,b,shift,dist,dx)
    rho1a=gaussian_cov(r1,shift)

    if (rho<=rho1a){
      plot_transio_data_mod(result[1:9,],result[10,],data,3)
    }
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


  tkgrid(win1$env$labelrange2, padx = 0.1, pady = c(10, 5))
  win1$env$sliderrange2 <- tk2scale(win1, from = 0.01, to = 2,
                                    variable = sliderValuerange2, orient = "horizontal", length = 500,
                                    command = onChange)
  tkgrid(win1$env$sliderrange2, padx = 0.1, pady = c(5, 10))


  tkgrid(win1$env$labelshift, padx = 0.1, pady = c(10, 5))
  win1$env$slidershift <- tk2scale(win1, from = -2, to = 2,
                                   variable = sliderValueshift, orient = "horizontal", length = 500,
                                   command = onChange)
  tkgrid(win1$env$slidershift, padx = 0.1, pady = c(5, 10))

  win1$env$butOK <-tk2button(win1, text = "OK", width = -6, command = onChange)
  tkgrid(win1$env$butOK, padx = 10, pady = c(5, 10))

}












