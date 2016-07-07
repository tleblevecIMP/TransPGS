#' correction when the proportion does not add to one
#' this can happens because of non-assigned values
#'

prop_correction_NA<-function(prop){
  return ( prop /sum(prop))
}
