#setup_package_data.r

#read in ecocrop database scraped from FAO website spring 2017
#EcoCrop_DB.csv

file_path <- system.file("extdata/", "EcoCrop_DB.csv", package = "climcropr")

df_ecocrop <- read.csv(file_path)

#devtools::use_data(df_ecocrop, overwrite=TRUE)

#df_ecocrop$COMNAME

# contains common names in lots of languages
# dividing the string by commas and taking the first item will
# work for at least the following crops
# maize, potato and rice

#potato
df_ecocrop$COMNAME[2231]
#trying and failing to split by comma and match first token

str_split(df_ecocrop$COMNAME, ",")[1] == 'potato'
#library(stringr)
filter( df_ecocrop, str_split(COMNAME, ",")[1] == 'potato')

cropname <- 'potato'
#could instead add on a comma and match that starting the string
#^ is regex for start of string

tst <- filter( df_ecocrop, str_detect(COMNAME, paste0("^",cropname,",")))

tst$PHMIN

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
