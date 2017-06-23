#setup_package_data.r

#read in ecocrop database scraped from FAO website spring 2017
#EcoCrop_DB.csv

file_path <- system.file("extdata/", "EcoCrop_DB.csv", package = "climcropr")

df_ecocrop <- read.csv(file_path)

#devtools::use_data(df_ecocrop)

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
