

library(XML)
library(stringr)
library(dplyr)
library(plyr)
library(RCurl)
library(jsonlite)
library(httr)
require(sp)
require(rgeos)
library(shotGroups)
library(data.table)
library(rgdal)

library(devtools)
library(roxygen2)
remove.packages("GNAFAPI")
#setwd("C:/")
#create("GNAFAPI")
setwd("C:/GNAFAPI")
document()
setwd("C:/")
install("GNAFAPI")
library(GNAFAPI)
library(GoogleAPI)
library(ABR)
setwd("C:/GNAFAPI")

detach("package:GNAFAPI", unload=TRUE)

shell("cd C:/gnaf C:/gnaf/src/main/script/run.sh", shell = "C:/Git/bin/sh.exe")

Quarter1 <- read.csv("file:///N:/ABR/Output 1 Dynamic Model/Exploration/19102016_Q1_MirrorAntijoin.csv",
                     stringsAsFactors = FALSE) %>%

            mutate(FULL_ADDRESS = gsub("/", " ", AddressLine1)) %>%
            mutate(FULL_ADDRESS = paste(FULL_ADDRESS, Suburb, Postcode, sep = " ")) %>%
            mutate(FULL_ADDRESS = ifelse(is.na(FULL_ADDRESS), "", FULL_ADDRESS)) %>%
            filter(!is.na(IMAP_X) & !is.na(IMAP_Y) & IMAP_X != 0 & IMAP_Y != 0)

##load in the VICMaps shapefile and run feed a sample of 1000 rows into the GNAF API
#VicMAPS <- readOGR("address.shp", layer = "address")

GNAF_VicMAPs <- GNAFAPI::VICMaps(VicMAPS)

boxplot(GNAF_VicMAPs$score)

##results look good
##Improvement: add address typing so that if the GNAF API can't find the correct unit it defaults to the site address

#run quarter1 on GNAF

GNAF <- Geocode(x=Quarter1$IMAP_X, y=Quarter1$IMAP_Y, id=Quarter1$MASTERID, FULL_ADDRESS=Quarter1$FULL_ADDRESS, radius=30, convert = TRUE)

#left Join up-to-date ANZSIC codes from ABR

CWW_DW <- SQLconnect()



ABR_df <- ABR::getABR(date = "20161024")



#put the addresses with multiple ANZICS through google API
remDr = GoogleAPI::StartServer()

remDr$open()

GNAF[] <- lapply(GNAF, as.character)

GNAF2 <- GNAF %>%
         select(.id,
                addressDetailPid,
                d61AddressNoAlias,
                score) %>%
         mutate(MASTERID = as.numeric(.id)) %>%
         full_join(Quarter1) %>%
         mutate(GNAFPID = addressDetailPid,
                EZI_ADD = ifelse(is.na(d61AddressNoAlias), FULL_ADDRESS, d61AddressNoAlias),
                EZI_ADD2 = gsub(" |/", "+", as.character(EZI_ADD)),
                URL = sapply(EZI_ADD2, PlaceURL)) %>%
        mutate(Place = sapply(URL, Place))


##Process the Google API results

Place <- GoogleAPI::ProcessResult(GNAF2)

##
GNAF2 <- GNAF %>%
  select(.id,
         addressDetailPid,
         d61AddressNoAlias,
         score) %>%
  mutate(MASTERID = as.numeric(.id)) %>%
  left_join(Quarter1) %>%
  mutate(GNAFPID = addressDetailPid) %>%
  left_join(select(ABRCWW,
                   GNAFPID,
                   LocationIndustryClass,
                   LocationIndustryClassDescription))

GNAF6 <- GNAF5 %>%
  select(.id,
         addressDetailPid,
         d61AddressNoAlias,
         score,
         FULL_ADDRESS,
         LocationIndustryClass,
         LocationIndustryClassDescription) %>%
  filter(!is.na(LocationIndustryClass)) %>%
  distinct()



