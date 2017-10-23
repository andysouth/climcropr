#' suit_soil_ph
#'
#' generate a global suitability (0/1) raster for a crop based on it's ph limits from ecocrop
#' and regridded harmonised world soil database v1.2 at 0.05 degree resolution
#'
#' Wieder, W.R., J. Boehnert, G.B. Bonan, and M. Langseth. 2014. Regridded Harmonized World Soil Database v1.2.
#' Data set. Available on-line [http://daac.ornl.gov] from Oak Ridge National Laboratory Distributed
#' Active Archive Center, Oak Ridge, Tennessee, USA. http://dx.doi.org/10.3334/ORNLDAAC/1247.
#'
#' @param cropname name of the crop
#' @param phmin min ph from ecocrop
#' @param phmax max ph from ecocrop
#' @param phopmn min optimal ph from ecocrop
#' @param phopmx max optimal ph from ecocrop
#' @param filename optional output file
#' @param suit_or_opt 'suit', 'opt', 'suit1_opt2'
#' @param plot whether to plot result
#'
#' @export
#'
# @importFrom dismo ecocrop, getCrop, ECOcrops
#' @import dismo raster ncdf4 stringr
#'
#' @examples
#' #takes a few mins to run
#' #rast_potato_suit_ph <- suit_soil_ph('potato')
#'
#'
suit_soil_ph <- function(cropname,
                         phmin = NULL,
                         phmax = NULL,
                         phopmn = NULL,
                         phopmx = NULL,
                         suit_or_opt = 'suit1_opt2',
                         filename = '',
                         plot = TRUE) {

  # function outline taken from f4 function in raster documentation
  # to be able to cope with large rasters not stored on disk
  # https://cran.r-project.org/web/packages/raster/vignettes/functions.pdf

  #TODO check args for suit_or_opt

  # optional outfile
  filename <- trim(filename)
  if (filename == '') filename <- rasterTmpFile()

  # get min & max ph from ecocrop
  # old way of offering suit or opt
  # if (is.null(min_ph))
  # {
  #   if (isTRUE(optimal)) min_ph <- get_ecocrop(cropname,'phopmn')
  #   else min_ph <- get_ecocrop(cropname,'phmin')
  # }
  #
  # if (is.null(max_ph))
  # {
  #   if (isTRUE(optimal)) max_ph <- get_ecocrop(cropname,'phopmx')
  #   else max_ph <- get_ecocrop(cropname,'phmax')
  # }

  if (is.null(phopmn)) phopmn <- get_ecocrop(cropname,'phopmn')
  if (is.null(phopmx)) phopmx <- get_ecocrop(cropname,'phopmx')
  if (is.null(phmin)) phmin <- get_ecocrop(cropname,'phmin')
  if (is.null(phmax)) phmax <- get_ecocrop(cropname,'phmax')


  # global soil ph data from regridded hwsd
  file_path <- system.file("extdata/", "T_PH_H2O.nc4", package = "climcropr")

   x <- raster(file_path)
   #output raster initially copied from input
   out <- x

   bs <- blockSize(out)
   out <- writeStart(out, filename, overwrite=TRUE)
   for (i in 1:bs$n) {
     v <- getValues(x, row=bs$row[i], nrows=bs$nrows[i] )

     # main line changedfrom f4 in the processing step
     # v <- v + a
     if (suit_or_opt == 'suit')
     {
       v <- ifelse( v >= phmin & v<= phmax, 1, 0)

     } else if (suit_or_opt == 'opt')
     {
       v <- ifelse( v >= phopmn & v<= phopmx, 1, 0)

     } else if (suit_or_opt == 'suit1_opt2')
     {
       #suitable=1, optimal=2
       v <- ifelse( v >= phopmn & v <= phopmx, 2,
            ifelse( v >= phmin & v <= phmax, 1, 0))
     }


     #TODO implement generic version of this with opmin,opmax,min,max


     out <- writeValues(out, v, bs$row[i])
     }
   out <- writeStop(out)

   # plot raster
   if (plot) plot(out, main = paste0("soil pH suitability ", cropname))

   invisible(out)
}
