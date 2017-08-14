#' within_t_thresh
#'
#' are a crop's temperature thresholds satisfied by the passed monthly temperature data ?
#'
#'
#' @param crop the crop either a name or ecocrop object
#' @param month_tmin min temp by month
#' @param month_tmax max temp by month
#' @param min_or_max 'both', min','max'
#'
#' @export
#'
#'
#' @examples
#' within_t_thresh(potato, month_tmin = rep(0,12), month_tmax = rep(50,12))

within_t_thresh <- function(crop,
                             month_tmin = NULL,
                             month_tmax = NULL,
                             min_or_max = 'both') {

  #todo : I should make it possible to pass T thresholds in addition to ecoccrop object

  if (class(crop) != "ECOCROPcrop")
  {
    #crop <- dismo::getCrop(crop)
    crop <- getCrop(crop)
  }


  in_thresh <- TRUE

  #todo check whether thresholds should be < or <=

  if ( (min_or_max=='both' | min_or_max=='min') & min(month_tmin) < crop@TMIN ) in_thresh <- FALSE

  if ( (min_or_max=='both' | min_or_max=='max') &  max(month_tmax) > crop@TMAX ) in_thresh <- FALSE

  return(in_thresh)
}
