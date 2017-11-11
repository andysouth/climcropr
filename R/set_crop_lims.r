#' set_crop_lims
#'
#' create a crop object with specified limits to allow suitability modelling
#'
#' @param GMIN minimum growth cycle in days
#' @param GMAX maximum growth cycle in days
#' @param KTMP killing temperature degrees C
#' @param TMIN min temperature degrees C
#' @param TMAX max temperature degrees C
#' @param RMIN min rainfall needed in growth cycle mm
#' @param RMAX max rainfall tolerated in growth cycle mm
#'
#'
#' @export
#'
#' @examples
#' made_up_crop <- set_crop_lims(GMIN=90, GMAX=120, KTMP=0, TMIN=10, TMAX=35, RMIN=500, RMAX=2000)
#' #test running suitability for small area
#' #subset climate data to make faster run
#' st_clim2 <- raster::crop(st_clim, extent(-10,10,45,65))
#' rastsuit <- ecocrop_a_raster(made_up_crop, st_clim2, simpler=TRUE)

set_crop_lims <- function(GMIN,
                          GMAX,
                          KTMP = 0,
                          TMIN,
                          TMAX,
                          RMIN,
                          RMAX) {

    #dismo - I would prefer not to be reliant on it
    crop <- new('ECOCROPcrop')

    crop@GMIN  <- GMIN
    crop@GMAX   <- GMAX
    crop@KTMP   <- KTMP
    crop@TMIN   <- TMIN
    #crop@TOPMN  <- TOPMN
    #crop@TOPMX  <- TOPMX
    crop@TMAX   <- TMAX
    crop@RMIN   <- RMIN
    #crop@ROPMN  <- ROPMN
    #crop@ROPMX  <- ROPMX
    crop@RMAX   <- RMAX

    #if no kill temp set it to 0
    #this is what dismo::ecocrop does
    #if (is.na(crop@KTMP)) crop@KTMP <- 0

  return(crop)
}
