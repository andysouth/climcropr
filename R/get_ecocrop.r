#' get_ecocrop
#'
#' get new ecocrop entry for a crop
#'
#' data scraped from FAO website 2017, see scraping script in data-raw/21_ExtractEcoCropSheets.R
#'
#' @param cropname an ecocrop cropname
#' @param field a field to select from the ecocrop database
#' @param ecocrop_object whether to return results as an ecocrop object default FALSE
#'
#' @import dplyr stringr
#'
#' @export
#'
#'
#' @examples
#' potato <- get_ecocrop('potato')
#' get_ecocrop('maize','phmin')
#' #comparing new & old versions of database
#' cropname <- 'maize'
#' library(dismo)
#' cropold <- dismo::getCrop(cropname)
#' cropnew <- get_ecocrop(cropname)

get_ecocrop <- function(cropname,
                        field = NULL,
                        ecocrop_object = FALSE) {

  data("df_ecocrop")

  #TODO add some warning about if field not present
  #TODO vectorise to work on a vector of crops

  # checking if the cropname appears as the first word in the COMNAME field

  #to test outside function
  #which(str_detect(df_ecocrop$COMNAME, regex(paste0("^",cropname,","), ignore_case = TRUE)))

  #case insensitive
  out <- dplyr::filter( df_ecocrop, str_detect(COMNAME, regex(paste0("^",cropname,","), ignore_case = TRUE)))

  if (nrow(out)==0) stop('crop ',cropname,' not found, check df_ecocrop$NAME')

  # do I want to offer option to return as an ecocrop object ?
  # e.g. to use within ecocrop_a_raster ??
  if (ecocrop_object)
  {
    #dismo - I would prefer not to be reliant on it
    crop <- new('ECOCROPcrop')

    crop@GMIN  <- as.numeric(out[,'GMIN'])
    crop@GMAX   <- as.numeric(out[,'GMAX'])
    crop@KTMP   <- as.numeric(out[,'KTMP'])
    crop@TMIN   <- as.numeric(out[,'TMIN'])
    crop@TOPMN  <- as.numeric(out[,'TOPMN'])
    crop@TOPMX  <- as.numeric(out[,'TOPMX'])
    crop@TMAX   <- as.numeric(out[,'TMAX'])
    crop@RMIN   <- as.numeric(out[,'RMIN'])
    crop@ROPMN  <- as.numeric(out[,'ROPMN'])
    crop@ROPMX  <- as.numeric(out[,'ROPMX'])
    crop@RMAX   <- as.numeric(out[,'RMAX'])

    #if no kill temp set it to 0
    #this is what dismo::ecocrop does
    if (is.na(crop@KTMP)) crop@KTMP <- 0

    return(crop)
  }


  #select just a single field if one is specified
  if (!is.null(field))
  {
    out <- dplyr::select(out, str_to_upper(field))
    #i could put something here to allow multiple fields to be returned
    #by only doing coversions below if a single field
    #if (length(field)==1)
    out <- out[[1]] #to return a single value rather than a dataframe
    #return factors as character
    if (is.factor(out)) out <- as.character(out)
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
