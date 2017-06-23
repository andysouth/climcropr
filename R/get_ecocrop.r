#' get_ecocrop
#'
#' get new ecocrop entry for a crop
#'
#' data scraped from FAO website 2017
#'
#' @param cropname an ecocrop cropname
#'
#' @export
#'
#'
#' @examples
#' potato <- get_ecocrop('potato')

get_ecocrop <- function(cropname) {

  #TODO add some warning about if crop not present

  filter( df_ecocrop, str_detect(COMNAME, paste0("^",cropname,",")))

}

#get_phmin('maize')
get_phmin <- function(cropname) {

  get_ecocrop(cropname)$PHMIN

}


get_phmax <- function(cropname) {

  get_ecocrop(cropname)$PHMAX

}
