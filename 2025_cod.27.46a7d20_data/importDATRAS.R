set.seed(1)

#install DTU Datras Package not icesDatras
#devtools::install_github("DTUAqua/DATRAS/DATRAS")
library(DATRAS) ## DTU package - should be used in conjunction with surveyIndex package and
## produces DATRASraw objects which is linked to the delta-GAM

## Species specific parameters:
years=1983:2025 #Update each year
genus="Gadus"
bfamily="morhua"

# below code doesn't work when trying to download directly from Datras, now manually download as below 
## Below can be used to extract raw exchange data from DATRAS i.e. combination of years and quarters to download of various surveys 
# icesDatras::getSurveyList() ## lists the available survey names on the DATRAS server
# NS <- getDatrasExchange("NS-IBTS", years=c(1983:2021), quarters=c(1,3), strict=FALSE)
# ScoGFS <- getDatrasExchange("SWC-IBTS", years=c(1985:2010), quarters=c(1,4), strict=FALSE)
# UKSGFS <- getDatrasExchange("SCOWCGFS", years=c(2011:2021), quarters=c(1,4), strict=FALSE)
# IRGFS <- getDatrasExchange("IE-IGFS", years=c(2003:2020), quarters=c(4), strict=FALSE)
## NOTE: strict = FALSE SHOULD be used: randomly allocates age data (CA) with missing haul IDs to hauls in either a) the same ICES rectangle or b) the same ICES roundfish area
## NOTE: strict = TRUE will DROP age records with missing haul IDs. 

# North Sea data downloaded manually.
# 2024-2025 data downloaded 7th April 2025
NS <- readExchangeDir(path = "data - Backup/datras/nsibts",pattern = ".zip", strict=FALSE)

# West of Scotland data downloaded manually.
# uksgfs 2024 & 2025  data downloaded 7 & 10 April 2025.
# irgfs 2024  data downloaded 7 April 2025.
# scogfs 1985-2010 copied over 10/04/25, downloaded via NW 18/10/22
ScoGFS <- readExchangeDir(path = "data - Backup/datras/scogfs",pattern = ".zip", strict=FALSE)
UKSGFS <- readExchangeDir(path = "data - Backup/datras/uksgfs",pattern = ".zip", strict=FALSE)
IRGFS <- readExchangeDir(path = "data - Backup/datras/irgfs",pattern = ".zip", strict=FALSE)

dAll <- c(NS,ScoGFS,UKSGFS,IRGFS)
dQ1=subset(dAll, Species==paste(genus,bfamily), Quarter==1, Year %in% years, HaulVal=="V", StdSpecRecCode==1)
dQ3=subset(dAll, Species==paste(genus,bfamily), Quarter==3, Year %in% years, HaulVal=="V", StdSpecRecCode==1)
dQ4=subset(dAll, Species==paste(genus,bfamily), Quarter==4, Year %in% years, HaulVal=="V", StdSpecRecCode==1)

save(dQ1,dQ3,dQ4, file="boot/initial/data/codData110425.Rdata")

dQ1 # view the data imported from Datras, should have 1 species, 7 countries, 8+ for Q34, data for every year, gear types gov and another one of 2. 