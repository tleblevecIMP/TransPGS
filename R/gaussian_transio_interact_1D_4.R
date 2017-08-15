#' gaussian interact for four facies
#'
gaussian_transio_interact_1D_4<-function(props,dist_vert,dx){
  windows()
  pF1=props[1]
  pF2 = props[2]
  pF3=props[3]
  pF4 = 1-pF1-pF2-pF3

  # this values are just for the initialization
  rho = 0
  t = threshold_fitting_4(pF1,pF2,pF3,rho,10)
  shift=0
  r1<-0.6
  r2<-0.2
  rho1a <- gaussian_cov(r1,shift)

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
      t<-threshold_fitting_4(pF1,pF2,pF3,rho,10)
    }

    result_vert<-transio_pgs_4(props,r1,r2,rho,t[1],t[2],shift,dist_vert,dx)
    rho1a=gaussian_cov(r1,shift)

    if (rho<=rho1a){
      plot_transio(result_vert[1:16,],result_vert[17,],4)
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

}
