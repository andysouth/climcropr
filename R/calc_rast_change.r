#' calc_rast_change
#' calculating maps of change, future-present is what we want, gain=1, loss=-1.
#' this function not really necessary can just do simple subtraction
#'
#' present		future 	f-p
#' 0		0	0
#' 0		1	1
#' 1		0	-1
#' 1		1	0
#'
#' @param now raster of now
#' @param fut raster of future
#'
#' @return raster where +1 = gain, -1 = loss
#' @export
# @examples


calc_rast_change <- function(now, fut) {

  change <- fut-now

  return(change)
}
