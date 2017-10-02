#setup_package_data.r

#read in ecocrop database scraped from FAO website spring 2017
#EcoCrop_DB.csv

file_path <- system.file("extdata/", "EcoCrop_DB.csv", package = "climcropr")

df_ecocrop <- read_csv(file_path)

#devtools::use_data(df_ecocrop, overwrite=TRUE)

#df_ecocrop$COMNAME

# contains common names in lots of languages
# dividing the string by commas and taking the first item will
# work for at least the following crops
# maize, potato and rice
# so I should create a new column, maybe just called NAME
df_ecocrop <- df_ecocrop %>%
  rowwise() %>%
  mutate(NAME= stringr::str_split(COMNAME, ",", simplify=TRUE)[1])


# read in list of Mirca names and equiv ecocrop names
file_path <- system.file("extdata/", "mirca_list.csv", package = "climcropr")
df_mirca <- read_csv(file_path)
#devtools::use_data(df_mirca, overwrite=TRUE)

## soil ph data
# from https://daac.ornl.gov/cgi-bin/dsviewer.pl?ds_id=546
# total available water capacity (mm water per 1 m soil depth),
# soil organic carbon density (kg C/m**2 for 0-30cm depth range),
# soil organic carbon density (kg C/m**2 for 0-100cm depth range),
# soil carbonate carbon density (kg C/m**2 for 0-100cm depth range),
# soil pH (0-30 cm depth range), and
# soil pH (30-100 cm depth range).

file_path <- system.file("extdata/", "wise_ph1.dat", package = "climcropr")

rst_soil_ph <- raster(file_path)

plot(rst_soil_ph)

# category  0 : Background
# category  1 : pH<=5.5
# category  2 : 5.5<pH<=7.3
# category  3 : 7.3<pH<=8.5
# category  4 : 8.5<pH
# category  5 : 4.0<pH<=8.5 Complex unit
# category  6 : Glaciers
# category  7 : Oceans

rst_soil_ph[rst_soil_ph==7 | rst_soil_ph==6 | rst_soil_ph==0] <- NA

image(rst_soil_ph)
library(viridis)
image(rst_soil_ph, col=viridis(5))

## Add a category column to the Raster Attribute Table
#rst_soil_ph <- as.factor(rst_soil_ph)
rst_soil_ph <- ratify(rst_soil_ph)
rat <- levels(rst_soil_ph)[[1]]
rat[["pH"]] <- c("<=5.5",">5.5 <=7.3", ">7.3 <=8.5", ">8.5","Complex unit")
levels(rst_soil_ph) <- rat

rasterVis::levelplot(rst_soil_ph, col.regions=rev(terrain.colors(5)), xlab="", ylab="")
rasterVis::levelplot(rst_soil_ph, col.regions=viridis(5), xlab="", ylab="")

# looking at higher resolution soil ph data
# regridded harmonised world soil database at 0.05 degree
# ncdf files from here  : https://daac.ornl.gov/SOILS/guides/HWSD.html


#file_path <- system.file("extdata/", "wise_ph1.dat", package = "climcropr")
file_path <- file.path("C:\\Dropbox\\ueaHelix2017\\soil\\HWSD_1247\\data\\T_PH_H2O.nc4")

rst_soil_ph2 <- raster(file_path)
plot(rst_soil_ph2)
hist(rst_soil_ph2)

# 0.5 degree country mask
file_path <- file.path("C:\\Dropbox\\ueaHelix2017\\country_mask\\ctry_reg_income_0.5_2013.nc")
rast_countries <- raster(file_path)
#plot(rast_c)
df_rast_countries <- data.frame(country_rst_num = raster::getValues(rast_countries), stringsAsFactors = FALSE)

df_c_lookup <- readr::read_table2("C:\\Dropbox\\ueaHelix2017\\country_mask\\gtap_gcam_income_2013.dat", col_names=FALSE)
names(df_c_lookup) <- c('country_rst_num','iso3c','country','region_numeric','region','x6')

#now left join df_c_lookup onto df_rast_countries
df_rast_c_lookup <- dplyr::left_join(df_rast_countries, df_c_lookup)

#todo add continent on here

#how it can be used to aggregate by region
#put into a function that will accept either a raster or a dataframe of 259200 rows

#now save this to the package
devtools::use_data(df_rast_c_lookup)

# putting example prediction data and corresponding Mirca data into package
# for 4 main crops + sorghum

# simpler suitability
maize_suitsimp <- ecocrop_a_raster('maize', st_clim, simpler=TRUE, rainfed=TRUE, diagnostic=FALSE)
rice_suitsimp <- ecocrop_a_raster('rice', st_clim, simpler=TRUE, rainfed=TRUE, diagnostic=FALSE)
wheat_suitsimp <- ecocrop_a_raster('wheat', st_clim, simpler=TRUE, rainfed=TRUE, diagnostic=FALSE)
soya_bean_suitsimp <- ecocrop_a_raster('soya bean', st_clim, simpler=TRUE, rainfed=TRUE, diagnostic=FALSE)
sorghum_suitsimp <- ecocrop_a_raster('broom-corn', st_clim, simpler=TRUE, rainfed=TRUE, diagnostic=FALSE)


# devtools::use_data(maize_suitsimp)
# devtools::use_data(rice_suitsimp)
# devtools::use_data(wheat_suitsimp)
# devtools::use_data(soya_bean_suitsimp)
# devtools::use_data(sorghum_suitsimp)

# ecocrop
maize_ecocrop <- ecocrop_a_raster('maize', st_clim, simpler=FALSE, rainfed=TRUE, diagnostic=FALSE)
rice_ecocrop <- ecocrop_a_raster('rice', st_clim, simpler=FALSE, rainfed=TRUE, diagnostic=FALSE)
wheat_ecocrop <- ecocrop_a_raster('wheat', st_clim, simpler=FALSE, rainfed=TRUE, diagnostic=FALSE)
soya_bean_ecocrop <- ecocrop_a_raster('soya bean', st_clim, simpler=FALSE, rainfed=TRUE, diagnostic=FALSE)
sorghum_ecocrop <- ecocrop_a_raster('broom-corn', st_clim, simpler=FALSE, rainfed=TRUE, diagnostic=FALSE)


# devtools::use_data(maize_ecocrop)
# devtools::use_data(rice_ecocrop)
# devtools::use_data(wheat_ecocrop)
# devtools::use_data(soya_bean_ecocrop)
# devtools::use_data(sorghum_ecocrop)

# mirca
# get_mirca not currently working
# temporary solution to read into package
folder <- 'C:\\Dropbox\\ueaHelix2017\\MIRCA\\_half_degree\\'
# mirca codes maize(2), wheat (1), rice (3), soybean (8)
maize_mirca <- raster(SDMTools::read.asc.gz(paste0(folder,'annual_area_harvested_rfc_crop02_ha_30mn.asc.gz')))
rice_mirca <- raster(SDMTools::read.asc.gz(paste0(folder,'annual_area_harvested_rfc_crop03_ha_30mn.asc.gz')))
wheat_mirca <- raster(SDMTools::read.asc.gz(paste0(folder,'annual_area_harvested_rfc_crop01_ha_30mn.asc.gz')))
soya_bean_mirca <- raster(SDMTools::read.asc.gz(paste0(folder,'annual_area_harvested_rfc_crop08_ha_30mn.asc.gz')))
sorghum_mirca <- raster(SDMTools::read.asc.gz(paste0(folder,'annual_area_harvested_rfc_crop07_ha_30mn.asc.gz')))



# stack all rasters into a single file (? or one file per crop)
stma <- raster::stack( maize_suitsimp, maize_ecocrop, maize_mirca )
# must be a better way of naming these
names(stma) <- c('maize_suitsimp', 'maize_ecocrop', 'maize_mirca')

stri <- raster::stack( rice_suitsimp, rice_ecocrop, rice_mirca )
# must be a better way of naming these
names(stri) <- c('rice_suitsimp', 'rice_ecocrop', 'rice_mirca')

stwh <- raster::stack( wheat_suitsimp, wheat_ecocrop, wheat_mirca )
# must be a better way of naming these
names(stwh) <- c('wheat_suitsimp', 'wheat_ecocrop', 'wheat_mirca')

stsb <- raster::stack( soya_bean_suitsimp, soya_bean_ecocrop, soya_bean_mirca )
# must be a better way of naming these
names(stsb) <- c('soya_bean_suitsimp', 'soya_bean_ecocrop', 'soya_bean_mirca')

stso <- raster::stack( sorghum_suitsimp, sorghum_ecocrop, sorghum_mirca )
# must be a better way of naming these
names(stso) <- c('sorghum_suitsimp', 'sorghum_ecocrop', 'sorghum_mirca')

st <- raster::stack(stma, stri, stwh, stsb, stso)

devtools::use_data(st, overwrite = TRUE)

