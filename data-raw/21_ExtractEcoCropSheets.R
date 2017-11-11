## ############################################################################
##
## TYNDALL CENTRE FOR CLIMATE CHANGE RESEARCH,
## SCHOOL OF ENVIRONMENTAL SCIENCES, UNIVERSITY OF EAST ANGLIA (UEA-TYN/ENV)
##
## ############################################################################
##
## DISCLAIMER:
## This script has been developed for research purposes only.
## The script is provided without any warranty of any kind, either express or
## implied. The entire risk arising out of the use or performance of the sample
## script and documentation remains with you. In no event shall UEA-ENV, its
## author, or anyone else involved in the creation, production, or delivery of
## the script be liable for any damages whatsoever (including, without
## limitation, damages for loss of business profits, business interruption,
## loss of business information, or other pecuniary loss) arising out of the use
## of or inability to use the sample scripts or documentation, even if UEA-ENV
## has been advised of the possibility of such damages.
##
## ############################################################################
##
## DESCRIPTION
## Sub-routine to extract the EcoCrop data sheets on '
## http://ecocrop.fao.org/
## Created on 16 May 2017
##
## Written by: Nicole Forstenh?usler
## For any problem with this code, please contact n.forstenhaeusler@uea.ac.uk
##
## ############################################################################
#rm(list = ls())

#--------------------------------------------------
# Load libraries
#--------------------------------------------------

library(XML)
my_dir <- "F:/ARCHIVE/Projects/HELIX/Data/Crops"

ecoCrop_list <- read.csv(file.path(my_dir, "EcoPort_CropList.csv"))

ecoDB <- data.frame(matrix(NA, nrow = NROW(ecoCrop_list), ncol = 54))
names(ecoDB) <- c("EcoPortCode", "AUTH", "FAMNAME", "SYNO", "COMNAME", "LIFO", "HABI", "LISPA", "PHYS", "CAT", "PLAT", "TOPMN", "TOPMX", "TMIN", "TMAX", "ROPMN", "ROPMX", "RMIN", "RMAX", "PHOPMN", "PHOPMX", "PHMIN", "PHMAX", "LATOPMN", "LATOPMX", "LATMN", "LATMX", "ALTMX", "LIOPMN", "LIOPMX", "LIMN", "LIMX", "DEP", "DEPR", "TEXT", "TEXTR", "FER", "FERR", "TOX", "TOXR", "SAL", "SALR", "DRA", "DRAR", "KTMPR", "KTMP", "PHOTO", "CLIZ", "ABITOL", "ABISUS", "INTRI", "PROSY", "GMIN", "GMAX")
for(i in 1153:NROW(ecoCrop_list)){
  ecoPortCode <- ecoCrop_list[i, "EcoPortCode"]
  ecoDB[i, "EcoPortCode"] <- ecoPortCode

  cropInfo <- readHTMLTable(paste0("http://ecocrop.fao.org/ecocrop/srv/en/cropView?id=", ecoPortCode), stringsAsFactors = FALSE)

  cropInfo <- cropInfo[[2]]
  ##Authority
  ecoDB[i, "AUTH"] <- names(cropInfo)[2]

  ##Family
  ecoDB[i, "FAMNAME"] <- cropInfo[1,2]

  ##Synonyms
  ecoDB[i, "SYNO"] <- cropInfo[2,2]

  ##Common names
  ecoDB[i, "COMNAME"] <- cropInfo[3,2]

  rm(cropInfo)

  cropSheet <- readHTMLTable(paste0("http://ecocrop.fao.org/ecocrop/srv/en/dataSheet?id=", ecoPortCode), stringsAsFactors = FALSE)


  description <- cropSheet[[1]]

  ##Life form
  ecoDB[i, "LIFO"] <- description[1, "V2"]

  ##Habitat
  ecoDB[i, "HABI"] <- description[2, "V2"]

  ##Life span
  ecoDB[i, "LISPA"] <- description[3, "V2"]

  ##Physiology
  ecoDB[i, "PHYS"] <- description[1, "V4"]

  ##Category
  ecoDB[i, "CAT"] <- description[2, "V4"]

  ##Plant attributes
  ecoDB[i, "PLAT"] <- description[3, "V4"]

  rm(description)

  #-----------------------------
  # Section: ECOLOGY
  #-----------------------------
  ecology <- cropSheet[[2]]

  ##Temperature
  ecoDB[i, "TOPMN"] <- as.numeric(ecology[ 3, "V2"])
  ecoDB[i, "TOPMX"] <- as.numeric(ecology[ 3, "V3"])
  ecoDB[i, "TMIN"] <- as.numeric(ecology[ 3, "V4"])
  ecoDB[i, "TMAX"] <- as.numeric(ecology[ 3, "V5"])

  ##Rainfall
  ecoDB[i, "ROPMN"] <- as.numeric(ecology[ 4, "V2"])
  ecoDB[i, "ROPMX"] <- as.numeric(ecology[ 4, "V3"])
  ecoDB[i, "RMIN"] <- as.numeric(ecology[ 4, "V4"])
  ecoDB[i, "RMAX"] <- as.numeric(ecology[ 4, "V5"])

  ##Soil PH
  ecoDB[i, "PHOPMN"] <- as.numeric(ecology[ 7, "V2"])
  ecoDB[i, "PHOPMX"] <- as.numeric(ecology[ 7, "V3"])
  ecoDB[i, "PHMIN"] <- as.numeric(ecology[ 7, "V4"])
  ecoDB[i, "PHMAX"] <- as.numeric(ecology[ 7, "V5"])

  ##Latitude (previously not listed)
  ecoDB[i, "LATOPMN"] <- as.numeric(ecology[5, "V2"])
  ecoDB[i, "LATOPMX"] <- as.numeric(ecology[5, "V3"])
  ecoDB[i, "LATMN"] <- as.numeric(ecology[5, "V4"])
  ecoDB[i, "LATMX"] <- as.numeric(ecology[5, "V5"])

  ##Altitude (previously not listed)
  ecoDB[i, "ALTMX"] <- as.numeric(ecology[6, "V5"])

  ##Light intensity
  ecoDB[i, "LIOPMN"] <- ecology[8, "V2"]
  ecoDB[i, "LIOPMX"] <- ecology[8, "V3"]
  ecoDB[i, "LIMN"] <- ecology[8, "V4"]
  ecoDB[i, "LIMX"] <- ecology[8, "V5"]

  ##Soil depth
  ecoDB[i, "DEP"] <- ecology[2, "V7"]
  ecoDB[i, "DEPR"] <- ecology[2, "V8"]

  ##Soil texture
  ecoDB[i, "TEXT"] <- ecology[3, "V7"]
  ecoDB[i, "TEXTR"] <- ecology[3, "V8"]

  ##Soil fertility
  ecoDB[i, "FER"] <- ecology[4, "V7"]
  ecoDB[i, "FERR"] <- ecology[4, "V8"]

  ##Soil Al. tox
  ecoDB[i, "TOX"] <- ecology[5, "V7"]
  ecoDB[i, "TOXR"] <- ecology[5, "V8"]

  ##Soil salinity
  ecoDB[i, "SAL"] <- ecology[6, "V7"]
  ecoDB[i, "SALR"] <- ecology[6, "V8"]

  ##Soil drainage
  ecoDB[i, "DRA"] <- ecology[7, "V7"]
  ecoDB[i, "DRAR"] <- ecology[7, "V8"]

  #-----------------------------
  # Section: CLIMATE
  #-----------------------------
  climateConditions <- cropSheet[[3]]

  ##Killing Temp during rest
  ecoDB[i, "KTMPR"] <- as.numeric(climateConditions[1,2])

  ##Killing Temp early growth
  ecoDB[i, "KTMP"] <- as.numeric(climateConditions[1,4])

  ##Photoperiod
  ecoDB[i, "PHOTO"] <- names(climateConditions)[4]

  ##Climate Zone
  ecoDB[i, "CLIZ"] <- names(climateConditions[2])

  ##Abiotic toler.
  ecoDB[i, "ABITOL"] <- climateConditions[2,2]

  ##Abiotic suscept.
  ecoDB[i, "ABISUS"] <- climateConditions[2,4]

  ##Introduction risk
  ecoDB[i, "INTRI"] <-climateConditions[3,2]

  rm(climateConditions)

  #-----------------------------
  # Section: CULTIVATION
  #-----------------------------
  cultivation <- cropSheet[[4]]

  ##Product. system
  ecoDB[i, "PROSY"] <- cultivation[2, "V2"]

  ##Crop Cycle
  ecoDB[i, "GMIN"] <- as.numeric(cultivation[cultivation[ ,"V3"] == "Crop cycle", "V4"])
  ecoDB[i, "GMAX"] <- as.numeric(cultivation[cultivation[ ,"V3"] == "Crop cycle", "V5"])

  rm(cultivation)
}

write.csv(ecoDB, file.path(my_dir, "EcoCrop_DB.csv"), row.names = FALSE)

## The names of the plants are not contained in the sheets, so merge to the names list
ecoDB <- read.csv(file.path(my_dir, "EcoCrop_DB.csv"), stringsAsFactors = FALSE)
namesList <- read.csv(file.path(my_dir, "EcoPort_CropList.csv"), stringsAsFactors = FALSE)

combined <- merge(namesList, ecoDB, by.x = "EcoPortCode", by.y = "EcoPortCode", all.y = TRUE)
write.csv(combined, file.path(my_dir, "EcoCrop_DB.csv"), row.names = FALSE)
