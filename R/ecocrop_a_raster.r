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
#' @param simpler trial of simpler suitability calc, done via cr_suit_simpler()
#' @param ... extra parameters to pass to writeRaster
#'
#' @export
#'
# @importFrom dismo ecocrop, getCrop, ECOcrops
#' @import dismo raster
#'
#' @examples
#' #subset climate data to make faster run
#' st_clim2 <- raster::crop(st_clim, extent(-10,10,0,60))
#' rast_potato_suit2 <- ecocrop_a_raster('potato',st_clim2,simpler=TRUE)
#' plot(rast_potato_suit2)
#' #world takes a few mins to run
#' #rast_potato_suit <- ecocrop_a_raster('potato',st_clim)
#' #st_potato_suit <- ecocrop_a_raster('potato',st_clim, simpler=TRUE, diagnostic=TRUE)
#'
ecocrop_a_raster <- function(crop,
                             st_clim_all = NULL,
                             st_tmin = NULL,
                             st_tavg = NULL,
                             st_prec = NULL,
                             rainfed = TRUE,
                             filename = NULL,
                             plot = TRUE,
                             simpler = TRUE,
                             diagnostic = TRUE, #think of better name for outputting suit by attribute
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

  if (!diagnostic)
  {
    outraster <- raster(st_tavg)
    v         <- vector(length = ncol(outraster))
  } #else
  {
    #setting up raster stack to receive diagnostic outputs
    #or just stack it afterwards
    outraster <- raster(st_tavg)

    #initialise rasters to receive results
    #must be a better way of doing this
    r_tmin <- r_tmax <- r_tkill <- r_rainhi <- r_rainlo <- r_all <- raster(st_tavg)
    v_tmin <- v_tmax <- v_tkill <- v_rainhi <- v_rainlo <- v_all <- vector(length = ncol(outraster))
  }


  #to receive more detailed results


  #for each row in the raster
  for (r in 1:nrow(outraster)){

    # andy added
    if (r%%10 == 1) message('row',r,' of ',nrow(outraster))

    if (!diagnostic) v[] <- NA
    else v_tmin <- v_tmax <- v_tkill <- v_rainhi <- v_rainlo <- v_all <- NA

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


      #only do calc if no NAs
      if(sum(is.na(clm)) == 0) {

        if (simpler) #new simpler trial suitability
        {
          #adding in a diagnostic option
          #to output a raster stack of all suitabilities
          if (!diagnostic)
          {
            v[i] <- cr_suit_simpler(crop = crop,
                                tmin = clm[, 1],
                                tavg = clm[, 2],
                                prec = clm[, 3],
                                rain = rainfed)
          } else
          {
            dfmonthsuit <- cr_suit_simpler_month(crop = crop,
                                    tmin = clm[, 1],
                                    tavg = clm[, 2],
                                    prec = clm[, 3],
                                    rain = rainfed)

            #put max monthly suit for each attribute
            v_tmin[i] <- max(dfmonthsuit$temp_min)
            v_tmax[i] <- max(dfmonthsuit$temp_max)
            v_tkill[i] <- max(dfmonthsuit$temp_kill)
            v_rainhi[i] <- max(dfmonthsuit$rain_high)
            v_rainlo[i] <- max(dfmonthsuit$rain_low)
            v_all[i] <- max(dfmonthsuit$all)
          }

        } else #version using dismp::ecocrop
        {
          e <- dismo::ecocrop(crop = crop,
                              tmin = clm[, 1],
                              tavg = clm[, 2],
                              prec = clm[, 3],
                              rain = rainfed)
          #v[i] <- e@maxper[1]
          # outputting max suitability, which is suit in the best month
          v[i] <- e@maxsuit[1]
          #e@suitability gives suit for each of 12 months
        }
      }
    }

    if (!diagnostic)
    {
      outraster[r, ] <- v
    } else
    {
      r_tmin[r, ] <- v_tmin
      r_tmax[r, ] <- v_tmax
      r_tkill[r, ] <- v_tkill
      r_rainhi[r, ] <- v_rainhi
      r_rainlo[r, ] <- v_rainlo
      r_all[r, ] <- v_all

    }

  } # end of rows loop

  if (diagnostic)
  {
    #stack the rasters
    outraster <- stack(r_tmin, r_tmax, r_tkill, r_rainhi, r_rainlo, r_all)
    #name the layers based on the datframe output from cr_suit_simpler_month()
    names(outraster) <- colnames(dfmonthsuit)
  }

  #andy move this  outside of the rows loop of the input raster
  if (!is.null(filename)) {
    filename  <- raster::trim(filename)
    outraster <- writeRaster(outraster, filename, ...)
  }

  if (plot) plot(outraster)

  return(outraster)
}
