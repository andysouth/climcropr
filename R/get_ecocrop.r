#' get_ecocrop
#'
#' get new ecocrop entry for a crop
#'
#' data scraped from FAO website 2017
#'
#' @param cropname an ecocrop cropname
#' @param field a filed to select
#'
#' @import dplyr stringr
#'
#' @export
#'
#'
#' @examples
#' potato <- get_ecocrop('potato')
#' get_ecocrop('maize','phmin')

get_ecocrop <- function(cropname,
                        field = NULL) {

  data("df_ecocrop")

  #TODO add some warning about if crop not present
  #TODO add some warning about if field not present
  #TODO can I vectorise to work on a vector of crops

  #to test outside function
  #which(str_detect(df_ecocrop$COMNAME, regex(paste0("^",cropname,","), ignore_case = TRUE)))

  #case insensitive
  out <- dplyr::filter( df_ecocrop, str_detect(COMNAME, regex(paste0("^",cropname,","), ignore_case = TRUE)))

  #select just a single field if one is specified
  if (!is.null(field))
  {
    out <- dplyr::select(out, str_to_upper(field))
    out <- out[[1]] #to return a single value rather than a dataframe
  }

  return(out)
}

# ph functions below, replaced by generic versions above
# #get_phmin('maize')
# get_phmin <- function(cropname) {
#
#   ph <- get_ecocrop(cropname)$PHMIN
#
#   # to protect against numeric(0)
#   if (length(ph)==0) ph <- NA
#
#   return(ph)
# }
#
#
# get_phmax <- function(cropname) {
#
#   ph <- get_ecocrop(cropname)$PHMAX
#
#   # to protect against numeric(0)
#   if (length(ph)==0) ph <- NA
#
#   return(ph)
# }
#
# get_phopmin <- function(cropname) {
#
#   ph <- get_ecocrop(cropname)$PHOPMN
#
#   # to protect against numeric(0)
#   if (length(ph)==0) ph <- NA
#
#   return(ph)
# }
#
#
# get_phopmax <- function(cropname) {
#
#   ph <- get_ecocrop(cropname)$PHOPMX
#
#   # to protect against numeric(0)
#   if (length(ph)==0) ph <- NA
#
#   return(ph)
# }
