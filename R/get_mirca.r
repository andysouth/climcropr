

library(raster)

zip_file <- "C:\\Dropbox\\ueaHelix2017\\MIRCA\\_half_degree\\annual_area_harvested_rfc_crop10_ha_30mn.asc.gz"
file_name <- "annual_area_harvested_rfc_crop10_ha_30mn"
destdir <- tempdir()

utils::unzip(zip_file, exdir=destdir)

rst <- raster::raster(file.path(destdir, file_name, paste0(file_name, '.asc')))

## this worked, beware default behaviour of gunzip is to remove file so it doesn't work a 2nd time
#zip_file <- "C:\\Dropbox\\ueaHelix2017\\MIRCA\\_half_degree\\annual_area_harvested_rfc_crop10_ha_30mn.asc.gz"
zip_file <- "C:\\Dropbox\\ueaHelix2017\\MIRCA\\_half_degree\\annual_area_harvested_rfc_crop01_ha_30mn.asc.gz"
library(R.utils)
rst <- raster(R.utils::gunzip(zip_file, remove=FALSE))

plot(rst)

#will it work directly from the ftp ? not yet ...
ftpfolder <- "ftp://ftp.rz.uni-frankfurt.de/pub/uni-frankfurt/physische_geographie/hydrologie/public/data/MIRCA2000/harvested_area_grids/"
file_name <- "annual_area_harvested_rfc_crop10_ha_30mn.asc.gz"
zip_file <- paste0(ftpfolder,file_name)

area(rst)
# class       : RasterLayer
# dimensions  : 360, 720, 259200  (nrow, ncol, ncell)
# resolution  : 0.5, 0.5  (x, y)
# extent      : -180, 180, -90, 90  (xmin, xmax, ymin, ymax)
# coord. ref. : NA
# data source : in memory
# names       : layer
# values      : 13.4722, 3077.24  (min, max)
