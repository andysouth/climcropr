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


get_mirca <- function(cropname,
                      rainfed = TRUE,
                      plot = TRUE) {

#todo move this somewhere else
df_crop <- read_csv("code,name
1, Wheat
2, Maize
3, Rice
4, Barley
5, Rye
6, Millet
7, Sorghum
8, Soybeans
9, Sunflower
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

raincode <- ifelse(rainfed,'rfc','irc')

#first without the gz
file_name <- paste0("annual_area_harvested_",raincode,"_crop",cropcode,"_ha_30mn.asc")
#file_name <- "annual_area_harvested_rfc_crop10_ha_30mn.asc"


folder <- "extdata/mirca"

## beware default behaviour of gunzip is to remove file so it doesn't work a 2nd time
# and  if the file has already been unzipped it fails

# still deciding best way to do this, could have the files unzipped already

file_path <- system.file(folder, file_name, package = "climcropr")

# if the asc file doesn't exist try adding the gz extension and unzipping
if (! file.exists(file_path))
{
  rst <- raster(R.utils::gunzip(paste0(file_path,".gz"), remove=FALSE))
} else
{
  rst <- raster(file_path)
}


if (plot) plot(rst)

#will it work directly from the ftp ? not yet ...
# ftpfolder <- "ftp://ftp.rz.uni-frankfurt.de/pub/uni-frankfurt/physische_geographie/hydrologie/public/data/MIRCA2000/harvested_area_grids/"
# file_name <- "annual_area_harvested_rfc_crop10_ha_30mn.asc.gz"
# zip_file <- paste0(ftpfolder,file_name)

invisible(rst)

}
