#' ecocrop_a_raster
#'
#' run ecocrop for all cells in a passed raster stack for the specified crop
#'
#' based on code from run_ecocrop() by ...
#'
#' @param crop the crop either a name or ecocrop object
#' @param st_tmin min temp by month in a raster stack
#' @param st_tavg mean temp by month in a raster stack
#' @param st_prec precipitation by month in a raster stack
#' @param rainfed default TRUE
#' @param filename optional output file
#'
#' @export
#'
#' @examples
#' #ecocrop_a_raster()

ecocrop_a_raster <- function(crop,
                             st_tmin,
                             st_tavg,
                             st_prec,
                             rainfed = TRUE,
                             filename = NULL,
                             ...) {

  if (class(crop) != "ECOCROPcrop")
    crop <- getCrop(crop)

  filename  <- trim(filename)

  outraster <- raster(st_tavg)
  v         <- vector(length = ncol(outraster))

  #for each row in the raster
  for (r in 1:nrow(outraster)){

    # andy added
    message('row',r,' of ',nrow(outraster))

    #a to receive results
    v[] <- NA

    tavg <- getValues(st_tavg, r)
    tmin <- getValues(st_tmin, r)

    if (rainfed) { prec <- getValues(st_prec, r)}

    # going through all NA cells (allows masking)
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
        e <- ecocrop(crop = crop, tmin = clm[, 1],
                     tavg = clm[, 2],
                     prec = clm[, 3],
                     rain = rainfed)
        #v[i] <- e@maxper[1]
        # outputting max suitability
        v[i] <- e@maxsuit[1]
      }
    }

    outraster[r, ] <- v
    #outr <- writeRaster(outr, filename, ...)
  }
  #andy tried moving this  outside of the rows loop of the input raster
  if (!is.null(filename))
    outraster <- writeRaster(outraster, filename, ...)

  return(outraster)
}
