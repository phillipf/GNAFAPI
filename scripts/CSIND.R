

library(GNAFAPI)
library(plyr)
library(dplyr)
CSIND <- read.csv("311016_CSIND_LGA_update.csv",
                  stringsAsFactors = FALSE) %>%
         mutate(FULL_ADDRESS = paste(gsub("/" ," ", STREET), LOCA_SUBURB, LOCA_POSTCODE, sep = " "))

GNAF <- Geocode(id=CSIND$Consumer, FULL_ADDRESS=CSIND$FULL_ADDRESS)

write.csv(GNAF, "311016_CSIND_Geocoded.csv")
