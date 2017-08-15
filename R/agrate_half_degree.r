#' agrate_half_degree
#'
#' to agregate a half degree grid to country or region, gives n and mean
#' Probably should go in another package, but here for a start.
#'
#' @param input either a raster of half degree cells or a data frame with 259200 cells in the order output by raster
#' @param ag_by 'country' or 'region' later can offer other options, e.g. 'continent'
# @param fun option for how to aggregate the data, e.g. mean or percent of country
#'
#' @export
#'
# @importFrom dismo ecocrop
#' @import raster dplyr
#'
#' @examples
#' #agrate_half_degree()
#' ec_out_potato <- raster(system.file("extdata", "ec_out_potato.grd", package = "climcropr"))
#' agrate_half_degree(ec_out_potato)
#'
agrate_half_degree <- function(input,
                               ag_by = 'region') {

  #todo check if its a raster object
  #if not don't need to do this
  if (class(input)=='raster' & length(ras)==259200)
     vals <- raster::getValues(input)
  else if (class(input)=='numeric' & length(input) == 259200)
    vals <- input
  else
    stop("input needs to be a half degree raster or sigle vector with 259200 rows")

  #add onto df_rast_c_lookup
  df_rast_c_lookup$new <- vals

  #agregate, initially just by region, works
  #todo : how to make this flexible to accept diff args for diff areas & diff aggregation

  if (ag_by =='region')
  {
    df_ag <- df_rast_c_lookup %>% group_by(region) %>%
      summarise(n = n(), mean = mean(new, na.rm = TRUE))

  } else if (ag_by =='country')
  {
    df_ag <- df_rast_c_lookup %>% group_by(country) %>%
      summarise(n = n(), mean = mean(new, na.rm = TRUE))
  }


  return(df_ag)
}
