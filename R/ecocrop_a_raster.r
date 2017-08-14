#' ecocrop_a_raster
#'
#' run ecocrop for all cells in a passed raster stack for the specified crop
#'
#' based on code from run_ecocrop() by Adam Sparks
#' CURRENTLY USES dismo::ecocrop() function, we may wish to refactor this to give us more control over
#'
#' @param crop the crop either a name or ecocrop object
#' @param st_clim_all single raster stack containing all climate inputs (see data(st_clim))
#' @param st_tmin min temp by month in a raster stack
#' @param st_tavg mean temp by month in a raster stack
#' @param st_prec precipitation by month in a raster stack
#' @param rainfed default TRUE
#' @param filename optional output file
#' @param ... extra parameters to pass to writeRaster
#'
#' @export
#'
# @importFrom dismo ecocrop, getCrop, ECOcrops
#' @import dismo raster
#'
#' @examples
#' #takes a few mins to run
#' #rast_potato_suit <- ecocrop_a_raster('potato',st_clim)

ecocrop_a_raster <- function(crop,
                             st_clim_all = NULL,
                             st_tmin = NULL,
                             st_tavg = NULL,
                             st_prec = NULL,
                             rainfed = TRUE,
                             filename = NULL,
                             ...) {

  if (class(crop) != "ECOCROPcrop")
  {
    #crop <- dismo::getCrop(crop)
    crop <- getCrop(crop)
  }


  #deriving tmin etc. raster stacks from single file if it provided
  if (!is.null(st_clim_all))
  {
    month_nums <- c(paste0('0',1:9),10:12)
    names_tmin <- paste0('on',month_nums)
    #names_tmax <- paste0('ox',month_nums)
    names_tavg <- paste0('oa',month_nums)
    names_prec <- paste0('op',month_nums)

    st_tmin <- raster::subset(st_clim_all, subset=names_tmin)
    #st_tmax <- raster::subset(st_clim_all, subset=names_tmax)
    st_tavg <- raster::subset(st_clim_all, subset=names_tavg)
    st_prec <- raster::subset(st_clim_all, subset=names_prec)
  }


  outraster <- raster(st_tavg)
  v         <- vector(length = ncol(outraster))

  #for each row in the raster
  for (r in 1:nrow(outraster)){

    # andy added
    if (r%%10 == 1) message('row',r,' of ',nrow(outraster))

    #a to receive results
    v[] <- NA

    tavg <- getValues(st_tavg, r)
    tmin <- getValues(st_tmin, r)

    if (rainfed) { prec <- getValues(st_prec, r)}

    # going through all non NA cells (allows masking)
    nac <- which(!is.na(tmin[, 1]))

    for (i in nac) {

      if (rainfed) {
        clm <- cbind(data.frame(tmin[i,]), tavg[i,],  prec[i,])
      } else {
        clm <- cbind(data.frame(tmin[i, ]), tavg[i, ])
      }

      #a? seems like it might fail if rainfed=FALSE due to clm[3]

      #only do calc if no NAs
      if(sum(is.na(clm)) == 0) {
        e <- dismo::ecocrop(crop = crop, tmin = clm[, 1],
                     tavg = clm[, 2],
                     prec = clm[, 3],
                     rain = rainfed)
        #v[i] <- e@maxper[1]
        # outputting max suitability, which is suit in the best month
        v[i] <- e@maxsuit[1]
        #TODO can I get it to output possible too ?
        #e@suitability gives suit for each of 12 months
      }
    }

    outraster[r, ] <- v
    #outr <- writeRaster(outr, filename, ...)
  }

  #andy move this  outside of the rows loop of the input raster
  if (!is.null(filename)) {
    filename  <- raster::trim(filename)
    outraster <- writeRaster(outraster, filename, ...)
  }

  return(outraster)
}
