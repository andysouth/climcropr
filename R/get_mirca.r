
#' @title Get a MIRCA2000 layer
#'
#' @description Download MIRCA2000 global data of monthly irrigated and rainfed
#' crop areas around the year 2000 (MIRCA2000)
#'
#' @param cropname a MIRCA2000 crop name, see \code{\link{df_crop}} for a full
#' list
#' @param rainfed default=TRUE, FALSE for irrigated
#' @param plot default=TRUE, plot the resulting raster
#' @param cache default=FALSE, TRUE to save MIRCA2000 files locally for future
#' use with \code{climcropr}
#'
#' @return A \code{\link[raster]{raster}} object of MIRCA2000 crop data and
#' optionally a gzipped file of the original data saved to local disk for future
#' use with \code{climcropr}.
#'
#' @examples
#' rst_mir <- get_mirca("potatoes", rainfed = TRUE)
#'
#' @references
#' The documentation for irrigated crops was published as Frankfurt Hydrology
#' Paper 06:
#'
#' Portmann, F., Siebert, S., Bauer, C. & Döll, P. (2008): Global data set of
#' monthly growing areas of 26 irrigated crops. Frankfurt Hydrology Paper 06,
#' Institute of Physical Geography, University of Frankfurt, Frankfurt am Main,
#' Germany.
#'
#' The documentation for both rainfed and irrigated crops is published in a
#' peer-reviewed journal:
#'
#' Portmann, F. T., Siebert, S. & Döll, P. (2010): MIRCA2000 – Global monthly
#' irrigated and rainfed crop areas around the year 2000: A new high-resolution
#' data set for agricultural and hydrological modeling, Global Biogeochemical
#' Cycles, 24, GB 1011, doi:10.1029/2008GB003435.
#'
#' For Frankfurt Hydrology Paper 09, the documentation for irrigated crops was
#' updated especially with respect to data quality and scaling. Data quality
#' parameters by country and unit, and crop calendars for rainfed and irrigated
#' crops for each unit are now available, too:
#'
#' Portmann, F.T. (2011): Global estimation of monthly irrigated and rainfed
#' crop areas on a 5 arc-minute grid. Frankfurt Hydrology Paper 09, Institute of
#' Physical Geography, University of Frankfurt, Frankfurt am Main, Germany.
#'
#'@seealso \code{\link{manage_cached_files}}
#'@importFrom raster plot
#'
#' @note
#' The data used in this package are downloaded from:
#'
#' \url{ftp://ftp.rz.uni-frankfurt.de/pub/uni-frankfurt/physische_geographie/hydrologie/public/data/MIRCA2000/harvested_area_grids/}
#'
#' @export
#'
get_mirca <- function(cropname,
                      rainfed = TRUE,
                      plot = TRUE,
                      cache = TRUE) {
  cropcode <-
    df_crop$code[toupper(cropname) == toupper(df_crop$name)]

  raincode <- ifelse(rainfed, "rfc", "irc")

  file_name <-
    paste0("annual_area_harvested_",
           raincode,
           "_crop",
           cropcode,
           "_ha_30mn.asc.gz")

  # check files that may exist locally in the cache_dir before downloading -----

  if (isTRUE(cache)) {
    cache_dir <- rappdirs::user_config_dir("climcropr")
    if (!file.exists(cache_dir)) {
      dir.create(cache_dir)
    }
  } else {
    cache_dir <- tempdir()
  }

  # filter downloaded ----------------------------------------------------------

  # which files are locally available?
  cache_dir_contents <-
    list.files(cache_dir, pattern = "asc.gz$")

  # which files requested need to be downloaded?
  dl_file <- file_name[!(file_name %in% cache_dir_contents)]

  # download files -------------------------------------------------------------
  if (length(dl_file) > 0) {
    message(" \nDownloading requested MIRCA file.\n ")

    MIRCA_ftp <-
      "ftp://ftp.rz.uni-frankfurt.de/pub/uni-frankfurt/physische_geographie/hydrologie/public/data/MIRCA2000/harvested_area_grids/"

    dl_file <- paste0(MIRCA_ftp, dl_file)

    tryCatch(
      download.file(
        url = unlist(dl_file),
        destfile = file.path(cache_dir, file_name),
        mode = "wb"
      ),
      error = function(x) {
        do.call(file.remove, list(list.files(cache_dir, full.names = TRUE)))
        stop("\nThe file download has failed.\n
             \nPlease start the download again.\n")
      }
    )
  }

  # add full file path to the file
  MIRCA_file <- file.path(cache_dir, file_name)

  # create a raster object of the file
  rst <-
    raster::raster(SDMTools::read.asc.gz(MIRCA_file))

  # plot the resulting object
  if (plot == TRUE) {
    plot(rst, main = paste0("MIRCA2000 ", cropname))
  }

  invisible(rst)

  }
