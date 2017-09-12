#' summ_half_degree
#'
#' to summarise a half degree grid to land area, country or region, gives n, mean and percent.
#' If you pass a 0/1 raster to it it will give the perecnt of cells that are 1.
#' Probably should go in another package, but here for a start.
#'
#' @param input either a raster of half degree cells or a data frame with 259200 cells in the order output by raster
#' @param by 'region', 'land' or 'country' - later can offer other options, e.g. 'continent'
# @param fun option for how to aggregate the data, e.g. mean or percent of country
#'
#' @export
#'
# @importFrom dismo ecocrop
#' @import raster dplyr
#'
#' @return dataframe with results summarised by region or country
#'
#' @examples
#' #summ_half_degree()
#' ec_out_potato <- raster(system.file("extdata", "ec_out_potato.grd", package = "climcropr"))
#' summ_half_degree(ec_out_potato)
#'
summ_half_degree <- function(input,
                             by = 'region') {

  #todo check if its a raster object
  #if not don't need to do this
  if (class(input)=='RasterLayer' & length(input)==259200)
     vals <- raster::getValues(input)
  else if (class(input)=='numeric' & length(input) == 259200)
    vals <- input
  else
    stop("input needs to be a half degree raster or single vector with 259200 rows")

  #add onto df_rast_c_lookup
  df_rast_c_lookup$new <- vals

  #agregate, initially just by region, works
  #todo : how to make this flexible to accept diff args for diff areas & diff aggregation

  if (by =='land')
  {
    df_ag <- df_rast_c_lookup %>% #group_by(region) %>%
      summarise(n = sum(!is.na(new)),
                mean = mean(new, na.rm = TRUE),
                percent = round(100*mean(new, na.rm = TRUE)) )

  }
  if (by =='region')
  {
    df_ag <- df_rast_c_lookup %>% group_by(region) %>%
      summarise(n = sum(!is.na(new)),
                mean = mean(new, na.rm = TRUE),
                percent = round(100*mean(new, na.rm = TRUE)) )

  } else if (by =='country')
  {
    df_ag <- df_rast_c_lookup %>% group_by(country) %>%
      summarise(n = sum(!is.na(new)),
                mean = mean(new, na.rm = TRUE),
                percent = round(100*mean(new, na.rm = TRUE)) )
  }


  return(df_ag)
}
