#' get_mirca
#'
#' get a mirca layer
#'
#' data from
#' ftp://ftp.rz.uni-frankfurt.de/pub/uni-frankfurt/physische_geographie/hydrologie/public/data/MIRCA2000/harvested_area_grids/
#'
#'
#' @param cropname a Mirca cropname
#' @param rainfed default=TRUE, FALSE for irrigated
#' @param plot whether to plot the raster
#'
#' @export
#'
# @importFrom dismo ecocrop, getCrop, ECOcrops
#' @import dismo raster readr R.utils
#'
#' @examples
#' rst_mir <- get_mirca(name_mir, rainfed = TRUE)
#' raster::plot(rst_mir)

get_mirca <- function(cropname,
                      rainfed = TRUE,
                      plot = TRUE) {

#todo move this somewhere else
df_crop <- read_csv("code,name
01, Wheat
02, Maize
03, Rice
04, Barley
05, Rye
06, Millet
07, Sorghum
08, Soybeans
09, Sunflower
10, Potatoes
11, Cassava
12, Sugar cane
13, Sugar beets
14, Oil palm
15, Rape seed
16, Groundnuts
17, Pulses
18, Citrus
19, Date palm
20, Grapes
21, Cotton
22, Cocoa
23, Coffee
24, Others perennial
25, Fodder grasses
26, Others")


cropcode <- df_crop$code[toupper(cropname) == toupper(df_crop$name)]

if (!(cropcode %in% c('01','02','07','10')))
{
  warning("temporarily Mirca data only downloaded for wheat, maize, sorghum and potatoes")
  return(FALSE)
}

raincode <- ifelse(rainfed,'rfc','irc')

#first without the gz BUT then failed if unzipped file did not exist
#file_name <- paste0("annual_area_harvested_",raincode,"_crop",cropcode,"_ha_30mn.asc")
file_name <- paste0("annual_area_harvested_",raincode,"_crop",cropcode,"_ha_30mn.asc.gz")
#file_name <- "annual_area_harvested_rfc_crop10_ha_30mn.asc.gz"

#annual_area_harvested_rfc_crop07_ha_30mn.asc.gz


folder <- "extdata/mirca"

## beware default behaviour of gunzip is to remove file so it doesn't work a 2nd time
# and  if the file has already been unzipped it fails
# so initially just unzip to temporary file and don't delete the gz

file_path <- system.file(folder, file_name, package = "climcropr")

rst <- raster(R.utils::gunzip(file_path, temporary=TRUE, remove=FALSE))

# if the asc file doesn't exist try adding the gz extension and unzipping
# if (! file.exists(file_path))
# {
#   rst <- raster(R.utils::gunzip(paste0(file_path,".gz"), temporary=TRUE, remove=FALSE))
# } else
# {
#   rst <- raster(file_path)
# }


if (plot) plot(rst)

#will it work directly from the ftp ? not yet ...
# ftpfolder <- "ftp://ftp.rz.uni-frankfurt.de/pub/uni-frankfurt/physische_geographie/hydrologie/public/data/MIRCA2000/harvested_area_grids/"
# file_name <- "annual_area_harvested_rfc_crop10_ha_30mn.asc.gz"
# zip_file <- paste0(ftpfolder,file_name)

invisible(rst)

}
