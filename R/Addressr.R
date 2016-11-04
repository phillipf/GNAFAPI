# Dynamic model
# Aim: Prepare existing data for dynamic segmentation

# Getting started ####
# Packages:
install.packages("rgdal")
install.packages("rgeos")
install.packages("maptools")
install.packages("stringr")
install.packages("raster")
install.packages("spdep")

library(dplyr)
library(tidyr)
library(ggplot2)
library(sp)
library(rgdal)
library(rgeos)
library(stringi)
library(stringr)
library(maptools)
library(RODBC)

#library(raster)
#library(spdep)

#detach("package:spdep", unload=TRUE)
#detach("package:raster", unload=TRUE)
#detach("package:data.table", unload=TRUE)
#detach("package:dtplyr", unload=TRUE)

# Functions:
# Data cleansing
capwords <- function(s, strict = FALSE) {
  cap <- function(s) paste(toupper(substring(s, 1, 1)),
                           {s <- substring(s, 2); if(strict) tolower(s) else s},
                           sep = "", collapse = " " )
  sapply(strsplit(s, split = " "), cap, USE.NAMES = !is.null(names(s)))
}
simpleCap <- function(x) {
  s <- strsplit(tolower(x) ,"[ ]|[-]|[*]")[[1]]
  paste0(toupper(substring(s, 1,1)), substring(s, 2),
         sep="", collapse=" ")
}
upperwords <- function(x) {
  s <- strsplit(tolower(x) ,"[ ]|[-]|[*]")[[1]]
  paste0(toupper(substring(s, 1,1)),
         sep="", collapse=" ")
}

captable <- function(v) {
  if (is.character(v)) return(toupper(v))
  else return(v)}

convert <- function(date) {
  result = as.POSIXct(strptime(date, format = "%Y%m%d"))
  return(result)
}

today <- Sys.Date()

# Connecting to internal databases
CWW_DW <- odbcDriverConnect(connection="Driver={SQL Server Native Client 11.0};
                            server=wvdb1devsql;
                            database=CWW_DW;
                            uid=Report;
                            pwd=report")
#
# Filtering aids (single source files) #### 
# Filter 1: Flat files of linked property coordinates
# Property: Original file - Static Snapshot
Property_dt <- read.csv("N:/KarlBlackhall/Gentrack AL/PROPERTY_LAYER_V2.csv", stringsAsFactors = FALSE) %>%
  as.data.table()

# Property: PP- Static Results of Property attached to Cadastre Attribute GID
Property_GID <- read.csv('N:/KarlBlackhall/Gentrack AL/Phil - DataValidation/PROPERTY_LAYER_OVERLAY.csv',
                         sep = ",", 
                         stringsAsFactors = FALSE)

# Filter 2: List of Barred ANZSIC Codes
ABR_ANZSIC_Bar <- read.csv("N:/ABR/Output 1 Dynamic Model/Realtime/22092016_ABR_ANZSIC_bar.csv", 
                           stringsAsFactors = FALSE) %>%
  left_join(select(ABR_Full, LocationIndustryClass, LocationIndustryClassDescription),
            by = c("LocationIndustryClass")) %>%
  group_by(LocationIndustryClass) %>%
  summarise(Description = paste0("",unlist(list(as.character(unique(LocationIndustryClassDescription)))), collapse = "; "),
            Count = n()) %>%
  left_join(select(Map_IndustryEndUse, ANZSIC, Guardian_description, Sector, Industry, Water_connect_status, EU_BlockWater),
            by = c("LocationIndustryClass" ="ANZSIC"))

# Filter 3: Spatial Supports 
# CWW Boundary Region
CWWBoundary <- readOGR("N:/Asset Information/MUNSYS MapInfo Data/Production/Data/CWW Boundary_2014_region.shp", 
                       layer="CWW Boundary_2014_region")

# Source 1: Australian Business Register (ABR) ####
ABR_Master <- sqlQuery(CWW_DW, "SELECT [PID]
                     ,[LocationType]
                     ,[LocationStartDate]
                     ,[AddressLine1]
                     ,[AddressLine2]
                     ,[Suburb]
                     ,[State]
                     ,[Postcode]
                     ,[CountryCd]
                     ,[DPID]
                     ,[Latitude]
                     ,[Longitude]
                     ,[Meshblock]
                     ,[GNAFPID]
                     ,[PositionalUncertaintyIndicator]
                     ,[AreaCode]
                     ,[PhoneNumber]
                     ,[AreaCodeMobile]
                     ,[MobileNumber]
                     ,[Email]
                     ,[LocationIndustryClass]
                     ,[LocationIndustryClassDescription]
                     FROM [CWW_DW].[dbo].[ABR_Businesslocation_20161024]
                     WHERE [STATE] = 'VIC'",
                     stringsAsFactors = FALSE) 

ABR_Full <- ABR_Master %>%
  filter(!is.na(GNAFPID),
         Latitude != 0)

ABR_Full$row = 1:nrow(ABR_Full)

ABR_DSP1 <- SpatialPointsDataFrame(coords = data.frame(x = ABR_Full$Longitude, 
                                                         y = ABR_Full$Latitude,
                                                         stringsAsFactors = FALSE),
                                     data=data.frame(PID = ABR_Full$row),
                                     proj4string= CRS("+proj=longlat +datum=WGS84")) %>%
  spTransform(CRS(proj4string(CWWBoundary))) 

ABR_Dynamic <- ABR_Full[which(gContains(CWWBoundary, ABR_DSP1, byid = TRUE)), ] %>%
  mutate(LocationStartDate = convert(LocationStartDate)) %>%
  subset.data.frame(!LocationIndustryClass %in% ABR_ANZSIC_Bar$LocationIndustryClass) %>%
  mutate(UID = paste0(GNAFPID,"_", PID),
         PID = as.character(PID)) %>%
  left_join(select(ABR_GNAF, PID, GNAF, LOCALITY_NAME),
                   by = c("PID")) %>%
  mutate(Full_Address = paste(GNAF, LOCALITY_NAME, Postcode))

ABR_Other <- ABR_Master %>%
  filter(is.na(GNAFPID)) %>%
  subset.data.frame(Suburb %in% ABR_Dynamic$Suburb|Suburb %in% ABR_GNAF$LOCALITY_NAME) %>%
  subset.data.frame(!LocationIndustryClass %in% ABR_ANZSIC_Bar$LocationIndustryClass)

ABR_Other$Suburb[8] %in% unique(ABR_Within$Suburb)

ABR_Dynamic[ABR_Dynamic$Suburb == "GEELONG",] %>% sample_n(1)

ABR_Dynamic_SP <- SpatialPointsDataFrame(coords = data.frame(x = ABR_Dynamic$Longitude, 
                                                             y = ABR_Dynamic$Latitude,
                                                             stringsAsFactors = FALSE),
                                         data=data.frame(PID = ABR_Dynamic$PID),
                                         proj4string= CRS("+proj=longlat +datum=WGS84"))  %>%
  spTransform(CRS(proj4string(CWWBoundary))) 
writeOGR(ABR_Dynamic_SP, ".", "ABR_Dynamic_SP", driver="ESRI Shapefile")

# Change over time - establishing the boundaries
ABR_Change1 <- select(ABR_Dynamic, GNAFPID, PID) %>%
  group_by(GNAFPID, PID) %>%
  summarise(n_GNAF = n(),
            n_PID = n(),
            PID_affected = paste0("",unlist(list(as.character(unique(PID)))), collapse = "; ")) 

ABR_Alternates <- ABR_Dynamic %>%
  filter(LocationType=='15')

ABR_Change2 <- ABR_Dynamic %>%
  subset.data.frame(PID %in% ABR_Alternates$PID)
write.table(ABR_Change2, "SNAP_LocationTypes.csv", sep = ",", row.names = FALSE)

ABR_Change3 <- ABR_Name %>%
  filter(PID == '32750713')

ABR_Change4 <- ABR_Dynamic %>%
  filter(PID == '32750713')

ABR_Beta <- select(ABR_Dynamic, PID, GNAFPID) %>%
  mutate(UID = paste0(GNAFPID,"_", PID)) %>% select(UID) %>% unique() %>%
  left_join(select(ABR_GNAF, GNAF, UID, LOCALITY_NAME, POSTCODE, CONFIDENCE, ALIAS_PRINCIPAL), 
            by = c("UID"))

ABR_TestDup <- ABR_Dynamic %>%
  mutate(UID = paste0(GNAFPID,"_", PID)) %>% 
  subset.data.frame(UID %in% ABR_Beta$UID[duplicated(ABR_Beta$UID)])
write.table(ABR_TestDup, "SNAP_UID_TestDup.csv", sep = ",", row.names = FALSE)

# 
# Exploration ABR: Testing of parameters 
# Summary of the current:
ABR_DynamicSummary <- select(ABR_Dynamic, PID, LocationIndustryClass, LocationIndustryClassDescription) %>%
  group_by(LocationIndustryClass) %>%
  summarise(Description = paste0("",unlist(list(as.character(unique(LocationIndustryClassDescription)))), collapse = "; "),
            n_Dynamic = n()) %>%
  left_join(select(Map_IndustryEndUse, ANZSIC, Guardian_description, Sector, Industry, Water_connect_status, EU_BlockWater),
            by = c("LocationIndustryClass" ="ANZSIC"))
 
ABR_Summary <- ABR_DynamicSummary %>%
  left_join(select(ABR_BaselineSummary, LocationIndustryClass, n_Baseline),
            by = c("LocationIndustryClass")) %>% 
  select(LocationIndustryClass, Description, 
         Guardian_description:EU_BlockWater,
         n_Dynamic, n_Baseline) %>%
  mutate(n_Diff = n_Dynamic - n_Baseline)

write.table(ABR_Summary, "09232016_ABR_IndustryCount_Mar2016.csv", sep = ",", row.names = FALSE) 

ABR_Attributes_AL1_Pf <- read.csv("N:/ABR/Output 1 Dynamic Model/Realtime/PP09262016_ABR_Test1_define.csv", 
                                  stringsAsFactors = FALSE) 
ABR_Attributes_AL1_Sf <- read.csv("N:/ABR/Output 1 Dynamic Model/Realtime/PP09262016_ABR_Test2b_define.csv", 
                                  stringsAsFactors = FALSE) 

# Addressr: Creating clean mirror of ABR addresses
addressr <- function(ABR_Select) {
ABR_Address <- data.frame(lapply(ABR_Select,captable)) %>%
  mutate(t1 = word(AddressLine1, 1),
         t1 = gsub("/ ", "/", t1),
         t2 = word(as.character(AddressLine1), -1),
         t8 = str_extract_all(t2, "[a-zA-Z]"),
         t8 = sapply(t8, paste, collapse=''),
         t3 = sapply(str_split(AddressLine1, " "), length),
         t9 = sapply(str_split(AddressLine2, " "), length),
         t10 = str_trim(AddressLine2, side = "both"),
         t10 = gsub("\\s*\\w*$", "" , AddressLine2),
         t12 = word(as.character(AddressLine2), -1),
         t5 = regexpr("[0-9]", AddressLine1),
         t5 = as.character(t5)) %>% select(-t2) %>%
  left_join(ABR_Attributes_AL1_Pf,
            by = c("t1")) %>%
  left_join(ABR_Attributes_AL1_Sf,
            by = c("t8")) %>%
  left_join(ABR_Attributes_AL1_Sf,
            by = c("t12"="t8")) %>%
  mutate(SAttribute4.x = ifelse(is.na(SAttribute4.x), "", SAttribute4.x),
         SAttribute4.y = ifelse(is.na(SAttribute4.y), "", SAttribute4.y)) %>%
  mutate(t6 = str_extract_all(AddressLine1, "[^a-zA-Z]"),
         t6 = sapply(t6, paste, collapse=''),
         t6 = gsub("\\'", "", t6),
         t6 = str_trim(t6, side = "both"),
         t6 = gsub(" / ", "/", t6),
         t6 = gsub(" & ", "&", t6),
         t6 = gsub("/", " ", t6),
         t7 = str_extract_all(t1, "[^a-zA-Z]"),
         t7 = sapply(t7, paste, collapse=''),
         t7 = as.character(str_match(t7, "[0-9]+[/][0-9]+")),
         n1 = word(t6,1),
         n1 = ifelse(PAttribute2=="UNIT"|PAttribute2=="PROPERTY",
                     n1,NA),
         n2 = word(t6,-1),
         n2 = ifelse(SAttribute4.x=="STREET"|SAttribute4.x=="AREA",
                     n2,NA),
         t10 = ifelse(PAttribute2=="UNIT", 
                      paste0(gsub("\\W+","",n1),"/",t10), 
                      t10),
         t11 = ifelse(t10 == "", 
                      paste(AddressLine1, AddressLine2),
                      t10)) %>%
  mutate(F6 = paste0(n1,"/",n2),
         F6 = ifelse(is.na(n1), n2,
                     ifelse(is.na(n2), 
                            n1, 
                            F6)),
         F7 = ifelse(is.na(t7), F6,
                     ifelse(is.na(F6)|F6=="[0-9]", 
                            t7, 
                            t7)),
         F1 = str_split(AddressLine1, "[0-9]"),
         F1 = sapply(F1, unlist),
         F1 = str_trim(sapply(F1, function(x) x[length(x)]),side = "both"),
         F11 = gsub("\\s*\\w*$", "" , F1),
         F11 = gsub("^([A-Z]){1}\\s(.+)", "\\2", F11),
         Clean = ifelse((t3 ==3&t5 ==1&(SAttribute4.x=="STREET"|SAttribute4.x=="AREA")),
                        (paste(t1,F11,SAttribute3.x)),
                        ifelse(t5 != -1&SAttribute4.x=="STREET"|SAttribute4.x=="AREA", 
                               paste(F7, F11, SAttribute3.x), 
                                NA)),
         Clean = ifelse(is.na(Clean), paste(t11, SAttribute3.y),
                                       Clean),
         Clean = ifelse(Clean == "NA NA", 
                        paste(AddressLine1,AddressLine2),
                                       Clean),
         C1 = sapply(str_split(Clean, " "), length),
         Clean = ifelse((C1 == 2|t5 == "-1"), 
                        paste(AddressLine1,AddressLine2),
                        Clean),
         Clean = str_extract_all(Clean, "\\d+\\D+"),
         Clean = sapply(Clean, unlist),
         Clean = sapply(Clean, paste, collapse=''),
         n3 = str_extract_all(Clean, "[^a-zA-Z]"),
         n3 = sapply(n3, paste, collapse=''),
         n3 = gsub(" / ", "/", n3),
         n3 = gsub(" & ", "&", n3),
         n3 = gsub("/", " ", n3),
         n3 = str_trim(n3, side = "both"),
         n3 = word(n3,-1),
         F12 = str_split(Clean, "[0-9]"),
         F12 = sapply(F12, unlist),
         F12 = str_trim(sapply(F12, function(x) x[length(x)]),side = "both"),
         F13 = sub('(.*)\\s+(\\S+)$', '\\1', F12),
         CALT2 = ifelse(word(Clean, -1) == t12, paste(n3,F13, SAttribute3.y), NA ),
         Clean = ifelse((C1 == 2|t5 == "-1"), 
                        CALT2,
                        Clean)) %>% 
  select(-C1)

ABR_Address <- select(ABR_Address, PID:Postcode, Clean) 

return(ABR_Address)
}

# First iteration (last updated on 3/10/2016) of Addressr output
ABR_Address <- select(ABR_Dynamic, PID, AddressLine1, AddressLine2:Postcode) %>%
  mutate(PID = as.character(PID)) %>%
  addressr
ABR_Address_Other <- select(ABR_Other, PID, AddressLine1, AddressLine2:Postcode) %>%
  addressr
write.table(ABR_Address_Other, "102016_ABR_Address_Other.csv", sep = ",", row.names = FALSE)

# Geocoded National Address File (G-NAF) for Clean ABR Addresses
# Run spatial packages for G-NAF
library(sp)  # vector data
library(raster)  # raster data
library(rgdal)  # input/output, projections
library(rgeos)  # geometry ops
library(spdep) # spatial dependence
library(lubridate)

# Inputs
GNAF_DefaultGeocode <- read.csv("N:/ABR/Output 1 Dynamic Model/Realtime/VIC_ADDRESS_DEFAULT_GEOCODE_psv.psv", 
                                sep = "|", stringsAsFactors = FALSE) 
GNAF_Detail <- read.csv("N:/ABR/Output 1 Dynamic Model/Realtime/VIC_ADDRESS_DETAIL_psv.psv", 
                        sep = "|", stringsAsFactors = FALSE) 
GNAF_StreetLocal <- read.csv("N:/ABR/Output 1 Dynamic Model/Realtime/VIC_STREET_LOCALITY_psv.psv", 
                             sep = "|", stringsAsFactors = FALSE) 
GNAF_Local <- read.csv("N:/ABR/Output 1 Dynamic Model/Realtime/VIC_LOCALITY_psv.psv", 
                       sep = "|", stringsAsFactors = FALSE) 

# Default Geocode
GNAF_DSP <- SpatialPointsDataFrame(coords = data.frame(x = GNAF_DefaultGeocode$LONGITUDE, 
                                                       y = GNAF_DefaultGeocode$LATITUDE,
                                                       stringsAsFactors = FALSE),
                                   data=data.frame(ADDRESS_DEFAULT_GEOCODE_PID = GNAF_DefaultGeocode$ADDRESS_DEFAULT_GEOCODE_PID),
                                   proj4string= CRS("+proj=longlat +datum=WGS84")) %>%
  spTransform(CRS(proj4string(CWWBoundary)))
GNAF_Within <- GNAF_DSP@data[which(gContains(CWWBoundary, GNAF_DSP, byid = TRUE)), ]
GNAF_DefaultGeocode <- GNAF_DefaultGeocode %>%
  subset.data.frame(ADDRESS_DEFAULT_GEOCODE_PID %in% GNAF_Within)
GNAF_DSP_data <- GNAF_DSP@data

ABR_GNAF <- select(ABR_Dynamic, PID, GNAFPID) %>%
  left_join(ABR_Address,
            by = c("PID")) %>%
  left_join(select(GNAF_Detail, LEVEL_NUMBER_PREFIX, ADDRESS_DETAIL_PID, FLAT_NUMBER, 
                   NUMBER_FIRST, NUMBER_LAST, STREET_LOCALITY_PID, LOCALITY_PID, POSTCODE,
                   CONFIDENCE, 
                   ALIAS_PRINCIPAL, PRIMARY_SECONDARY),
            by = c("GNAFPID" = "ADDRESS_DETAIL_PID")) %>%
  mutate(FINAL_NUM = ifelse(!is.na(NUMBER_LAST), paste0(NUMBER_FIRST, "-", NUMBER_LAST),
                            ifelse(is.na(NUMBER_LAST), NUMBER_FIRST,
                                   NUMBER_LAST)),
         FINAL_NUM = ifelse(!is.na(FLAT_NUMBER), paste0(FLAT_NUMBER, "/",FINAL_NUM),
                            ifelse(is.na(FLAT_NUMBER), FINAL_NUM,
                                   NUMBER_FIRST))) %>%
  left_join(select(GNAF_StreetLocal, STREET_LOCALITY_PID, STREET_NAME, STREET_TYPE_CODE),
            by = c("STREET_LOCALITY_PID")) %>%
  mutate(GNAF = paste(FINAL_NUM, STREET_NAME, STREET_TYPE_CODE)) %>% 
  left_join(select(GNAF_Local, LOCALITY_PID, LOCALITY_NAME),
            by = c("LOCALITY_PID")) %>%
  select(PID:Clean, LEVEL_NUMBER_PREFIX, GNAF, LOCALITY_NAME, POSTCODE, CONFIDENCE, ALIAS_PRINCIPAL, PRIMARY_SECONDARY) %>%
  mutate(UID = paste0(GNAFPID,"_", PID)) %>% unique()

ABR_Level <- ABR_Attributes_AL1_Pf %>%
  filter(PAttribute2 == "LEVEL") %>%
  select(t1) %>%
  mutate(t1 = sapply(t1, width = 1, side = "both", pad = " ",str_pad))

ABR_GNAF[ABR_GNAF ==""] <- NA

match <- function(x) {
  y <- strsplit(x, " ") 
  if(length(y) > 0) {any(sapply(y, '%in%', unname(ABR_Level$t1)))
  }
  else {
    NA
  }
}

ABR_GNAF[] <- lapply(ABR_GNAF, as.character)
ABR_GNAF$ADD1 = sapply(ABR_GNAF$AddressLine1, match)
ABR_GNAF$ADD2 = sapply(ABR_GNAF$AddressLine2, match)

# Other Processes: 
convert <- function(date) {
  result = as.POSIXct(strptime(date, format = "%Y%m%d"))
  return(result)
}
ABR_Name <- sqlQuery(CWW_DW, "SELECT [PID]
                     ,[ABN]
                     ,[EntityType]
                     ,[EntityOrganisationName]
                     ,[ABNRegDOE]
                     ,[ABNCanDOE]
                     ,[MainTradingName]
                     FROM [CWW_DW].[dbo].[ABR_Agency_Data_20160315]",
                     stringsAsFactors = FALSE) %>% 
            subset.data.frame(PID %in% ABR_Dynamic$PID) %>%
            mutate(ABNRegDOE = convert(ABNRegDOE),
                   ABNCanDOE = convert(ABNCanDOE)) %>%
            arrange(desc(ABNCanDOE)) %>%
  mutate(Status = ifelse(!is.na(ABNCanDOE), "Cancelled",
                         "Active"),
         LastUpdate = Sys.Date(),
         Months = (as.POSIXct(today) - ABNRegDOE)/30.5,
         Months = ifelse(is.na(ABNCanDOE),
                         round(Months, digit=0),
                         NA)) %>%
  mutate(UID = paste0(GNAFPID,"_", PID))

# Test scenario 1: Multiples in ABR (test beta - not functional - further develop for dynamic)
Match_multiples <- read.csv('G:/Business and Environmental Services/Business Customer Programs/Customers/Master Lists/Significant Water Users for Quarterly Reporting/Q1 2016-17/Match/261016_GNAFANZSIC2.csv',
                            sep = ",", 
                            stringsAsFactors=FALSE) %>%
  filter(!is.na(LocationIndustryClass)) %>%
  select(MASTERID, GNAFPID, LocationIndustryClass) %>%
  mutate(C1 = substr(LocationIndustryClass, 1,1),
         C2 = substr(LocationIndustryClass, 1,2),
         C3 = substr(LocationIndustryClass, 1,3)) %>%
  group_by(GNAFPID, MASTERID) %>%
  summarise(sample_n = n(), 
            Class1 = paste0("",unlist(list(as.character(unique(C1)))), collapse = "; "),
            Class2 = paste0("",unlist(list(as.character(unique(C2)))), collapse = "; "),
            Class3 = paste0("",unlist(list(as.character(unique(C3)))), collapse = "; ")) %>%
  mutate(Class1_n = sapply(str_split(Class1, "; "), length),
         Class2_n = sapply(str_split(Class2, "; "), length),
         Class3_n = sapply(str_split(Class3, "; "), length)) %>%
  select(-Class3)

# Test scenario 2: 


#
# Source 2: Critical CWW customers ####
#
# Source 3: VICMaps: Land Use Codes ####
#
# Source 4: Sensis Purchase ####
Sensis <- sqlQuery(CWW_DW, 
                   "SELECT DISTINCT [ID]
			             ,[anzsic]
                   ,[anzsic_desc]
                   ,[yellow]
                   ,[F18]
                   FROM [CWW_DW].[dbo].[NONRESMETERS_ANZSIC_CODES_Matched]", 
                   stringsAsFactors = FALSE) %>%
  mutate(MASTERID = as.numeric(substring(ID, 3, 9)),
         ANZSICSENSIS = anzsic)

# Appendix ####
# Appendix 1a: Creating the list of catalogue of ABR ANZSIC codes for interpretation
ABR_ANZSIC <- select(ABR_Full, LocationIndustryClass, LocationIndustryClassDescription, PID) %>%
  filter(!is.na(LocationIndustryClass)) %>%
  group_by(LocationIndustryClass) %>%
  summarise(Description = paste0("",unlist(list(as.character(unique(LocationIndustryClassDescription)))), collapse = "; "),
            Count = n()) %>%
  left_join(select(Map_IndustryEndUse, ANZSIC, Guardian_description, Sector, Industry, Water_connect_status, EU_BlockWater),
            by = c("LocationIndustryClass" ="ANZSIC"))
write.table(ABR_ANZSIC_Bar, "22092016_ABR_ANZSIC_bar.csv", sep = ",", row.names = FALSE)  

# Appendix 1b: Subset List of barred customers
ABR_ANZSIC_Bar <- ABR_ANZSIC %>%
  filter(Count > 10000) %>%
  rbind(ABR_ANZSIC[c(505,506,507), ])
ABR_ANZSIC_Bar <- ABR_ANZSIC_Bar[-c(4), ]
write.table(ABR_ANZSIC, "13072016_ABR_ANZSIC.csv", sep = ",", row.names = FALSE)  

# Appendix 1c: Revised real-time subset list of barred customers (combining another list of service and activity industry codes)
ABR_ANZSIC_Bar <- read.csv("N:/ABR/Output 1 Dynamic Model/Realtime/22092016_ABR_ANZSIC_bar.csv", 
                           stringsAsFactors = FALSE) %>%
  left_join(select(ABR_Full, LocationIndustryClass, LocationIndustryClassDescription),
            by = c("LocationIndustryClass")) %>%
  group_by(LocationIndustryClass) %>%
  summarise(Description = paste0("",unlist(list(as.character(unique(LocationIndustryClassDescription)))), collapse = "; "),
            Count = n()) %>%
  left_join(select(Map_IndustryEndUse, ANZSIC, Guardian_description, Sector, Industry, Water_connect_status, EU_BlockWater),
            by = c("LocationIndustryClass" ="ANZSIC"))
  
# Appendix 2: First glance at data. This clipping method is no longer used, the method had been refined, below now superceded.
# 2a) CWW Prop location clippings for crude first-cut filtering
QCLIP_CATCHMENT <- sqlQuery(CWW_DW, "SELECT DISTINCT [LOCA_SUBURB]
                            ,[LOCA_POSTCODE]
                            ,[LGA]
                            FROM [CWW_DW].[dbo].[GENTRACK_WaterEfficiency_PROP_CONSUMP]",
                            stringsAsFactors = FALSE) %>%
  filter(!is.na(LOCA_SUBURB),
         !is.na(LOCA_POSTCODE),
         !is.na(LGA))

# 2b) ABR clipped by CWW Prop Suburbs
ABR_Filter <- sqlQuery(CWW_DW, "SELECT [PID]
                        ,[LocationType]
                        ,[LocationStartDate]
                        ,[AddressLine1]
                        ,[AddressLine2]
                        ,[Suburb]
                        ,[State]
                        ,[Postcode]
                        ,[CountryCd]
                        ,[DPID]
                        ,[Latitude]
                        ,[Longitude]
                        ,[Meshblock]
                        ,[GNAFPID]
                        ,[PositionalUncertaintyIndicator]
                        ,[AreaCode]
                        ,[PhoneNumber]
                        ,[AreaCodeMobile]
                        ,[MobileNumber]
                        ,[Email]
                        ,[LocationIndustryClass]
                        ,[LocationIndustryClassDescription]
                        FROM [CWW_DW].[dbo].[ABR_Businesslocation_20160315]",
                       stringsAsFactors = FALSE) %>%
  filter(State == 'VIC',
         !is.na(Longitude),
         Longitude != "0") %>%
  mutate(AddressLine1 = sapply(AddressLine1, toupper),
         AddressLine2 = sapply(AddressLine2, toupper),
         Suburb = sapply(Suburb, toupper)) %>%
  subset.data.frame(Suburb %in% QCLIP_CATCHMENT$LOCA_SUBURB|
                      Postcode %in% QCLIP_CATCHMENT$LOCA_POSTCODE)
rm(ABR_Filter) 

# Appendix 3: Before snapshot of industries
ABR_Before <- sqlQuery(CWW_DW, "SELECT [PID]
                        ,[LocationType]
                     ,[LocationStartDate]
                     ,[AddressLine1]
                     ,[AddressLine2]
                     ,[Suburb]
                     ,[State]
                     ,[Postcode]
                     ,[CountryCd]
                     ,[DPID]
                     ,[Latitude]
                     ,[Longitude]
                     ,[Meshblock]
                     ,[GNAFPID]
                     ,[PositionalUncertaintyIndicator]
                     ,[AreaCode]
                     ,[PhoneNumber]
                     ,[AreaCodeMobile]
                     ,[MobileNumber]
                     ,[Email]
                     ,[LocationIndustryClass]
                     ,[LocationIndustryClassDescription]
                     FROM [CWW_DW].[dbo].[ABR_Businesslocation]",
                     stringsAsFactors = FALSE) %>%
  filter(State == 'VIC',
         !is.na(Longitude),
         Longitude != "0")

ABR_BDSP2 <- SpatialPointsDataFrame(coords = data.frame(x = ABR_Before$Longitude, 
                                                       y = ABR_Before$Latitude,
                                                       stringsAsFactors = FALSE),
                                   data=data.frame(PID = ABR_Before$PID),
                                   proj4string= CRS("+proj=longlat +datum=WGS84")) %>%
  spTransform(CRS(proj4string(CWWBoundary))) 

ABR_BWithin <- ABR_BDSP2@data[which(gContains(CWWBoundary, ABR_BDSP2, byid = TRUE)), ]

ABR_Baseline <- ABR_Before %>%
  subset.data.frame(PID %in% ABR_BWithin) %>%
  mutate(LocationStartDate = ifelse(is.na(LocationStartDate),
                                    NA,
                                    paste0(substring(LocationStartDate, 1,4), "-", 
                                           substring(LocationStartDate, 5,6),"-",substring(LocationStartDate, 7,8))),
         LocationStartDate = as.Date(LocationStartDate)) %>%
  subset.data.frame(!LocationIndustryClass %in% ABR_ANZSIC_Bar$LocationIndustryClass) %>%
  mutate(Full_Address = ifelse(is.na(AddressLine2),
                               paste(AddressLine1,Suburb, Postcode),
                               paste(AddressLine1, AddressLine2, Suburb, Postcode)))

ABR_BaselineSummary <- select(ABR_Baseline, PID, LocationIndustryClass, LocationIndustryClassDescription) %>%
  group_by(LocationIndustryClass) %>%
  summarise(Description = paste0("",unlist(list(as.character(unique(LocationIndustryClassDescription)))), collapse = "; "),
            n_Baseline = n()) %>%
  left_join(select(Map_IndustryEndUse, ANZSIC, Guardian_description, Sector, Industry, Water_connect_status, EU_BlockWater),
            by = c("LocationIndustryClass" ="ANZSIC"))
rm(ABR_Before)
rm(ABR_BWithin)
rm(ABR_BDSP2)

# Appendix 4: a) Supporting ABR Data Attributes: Creation of Rules
ABR_Test <- data.frame(lapply(ABR_Dynamic, function(v) {
  if (is.character(v)) return(toupper(v))
  else return(v)})) %>%
  select(AddressLine1, AddressLine2) %>%
  mutate(t1 = word(AddressLine1, 1),
         t2 = paste(word(AddressLine1, -2),(word(AddressLine1, -1))),
         t3 = word(AddressLine2, 1),
         t4 = word(AddressLine2, -1))

ABR_Test1 <- select(ABR_Test, AddressLine1, t1, t2) %>% 
  mutate(t3 = sapply(str_split(AddressLine1, " "), length),
         t4 = grepl("[0-9]", AddressLine1, TRUE),
         t5 = regexpr("[0-9]", AddressLine1),
         t6 = str_extract_all(AddressLine1, "[^a-zA-Z]"),
         t6 = sapply(t6, paste, collapse=''),
         t6 = gsub("\\'", "", t6),
         t7 = str_extract_all(AddressLine1, "[^a-zA-Z0-9]"),
         t7 = sapply(t7, paste, collapse=''))

ABR_Test1_define <- ABR_Test1 %>%
  filter(t4 == TRUE) %>%
  filter(t5>1) %>%
  filter(t3 == "2") %>% select(t1) %>% 
  mutate(t1 = str_extract_all(t1, "[a-zA-Z]"),
         t1 = sapply(t1, paste, collapse=''),
         t1 = gsub("\\'", "", t1)) %>%
  unique()

write.table(ABR_Test1_define, "09262016_ABR_Test1_define.csv", sep = ",", row.names = FALSE)

ABR_Test2_define <- data.frame(lapply(ABR_Dynamic, function(v) {
  if (is.character(v)) return(toupper(v))
  else return(v)})) %>%
  select(AddressLine1, AddressLine2) %>%
  mutate(t2 = word(AddressLine1, -1),
         t3 = sapply(str_split(AddressLine1, " "), length),
         t5 = regexpr("[0-9]", AddressLine1)) %>%
  filter(t5 != "-1" & t3 != "1") %>%
  mutate(t8 = str_extract_all(t2, "[a-zA-Z]"),
         t8 = sapply(t8, paste, collapse='')) %>% 
  distinct()

ABR_Test2a_define <- select(ABR_Test2_define, t3, t5, t8) %>%
  unique()

write.table(ABR_Test2a_define, "09262016_ABR_Test2a_define.csv", sep = ",", row.names = FALSE)


# b) Execution of test scenerio for migration 
ABR_Attributes_AL1_Pf <- read.csv("N:/ABR/Output 1 Dynamic Model/Realtime/PP09262016_ABR_Test1_define.csv", 
                                  stringsAsFactors = FALSE) 
ABR_Attributes_AL1_Sf <- read.csv("N:/ABR/Output 1 Dynamic Model/Realtime/PP09262016_ABR_Test2b_define.csv", 
                                  stringsAsFactors = FALSE) 

ABR_Select <- select(ABR_Dynamic, PID, AddressLine1, AddressLine2:Postcode)
ABR_Address <- data.frame(lapply(ABR_Select,captable)) %>%
  mutate(t1 = gsub("/ ", "/", t1),
         t1 = word(as.character(AddressLine1), 1),
         t2 = word(as.character(AddressLine1), -1),
         t8 = str_extract_all(t2, "[a-zA-Z]"),
         t8 = sapply(t8, paste, collapse=''),
         t3 = sapply(str_split(AddressLine1, " "), length),
         t5 = regexpr("[0-9]", AddressLine1),
         t5 = as.character(t5)) %>% select(-t2) %>%
  left_join(ABR_Attributes_AL1_Pf,
            by = c("t1")) %>%
  left_join(ABR_Attributes_AL1_Sf,
            by = c("t8")) %>%
  mutate(t6 = str_extract_all(AddressLine1, "[^a-zA-Z]"),
         t6 = sapply(t6, paste, collapse=''),
         t6 = gsub("\\'", "", t6),
         t9 = word(ABR_Address$AddressLine1, -2),
         t6 = str_trim(t6, side = "both"),
         t6 = gsub(" / ", "/", t6),
         t6 = gsub(" & ", "&", t6),
         t6 = gsub("/", " ", t6),
         t7 = str_extract_all(t1, "[^a-zA-Z]"),
         t7 = sapply(t7, paste, collapse=''),
         t7 = as.character(str_match(t7, "[0-9]+[/][0-9]+")),
         n1 = word(t6,1),
         n1 = ifelse(PAttribute2=="UNIT"|PAttribute2=="PROPERTY",
                     n1,NA),
         n2 = word(t6,-1),
         n2 = ifelse(SAttribute4=="STREET"|SAttribute4=="AREA",
                     n2,NA),
         n3 = word(t6,-2),
         n3 = ifelse(n3==n1,NA, n3)) %>%
  mutate(F6 = paste0(n1,"/",n2),
         F6 = ifelse(is.na(n1), n2,
                     ifelse(is.na(n2), 
                            n1, 
                            F6)),
         F7 = ifelse(is.na(t7), F6,
                     ifelse(is.na(F6)|F6=="[0-9]", 
                            t7, 
                            t7)),
         F1 = str_split(AddressLine1, "[0-9]"),
         F1 = sapply(F1, unlist),
         F1 = str_trim(sapply(F1, function(x) x[length(x)]),side = "both"),
         F11 = gsub("\\s*\\w*$", "" , F1),
         Clean = ifelse((t3 ==3&t5 ==1&(SAttribute4=="STREET"|SAttribute4=="AREA")),
                        (paste(t1,t9,SAttribute3)),
                        ifelse(t5 != -1&t3!=3&t5!=1&SAttribute4=="STREET"|SAttribute4=="AREA", 
                               paste(F7, F11, SAttribute3), 
                               NA)))

# Appendix 5: Creating a Package, Addressr
library(devtools)
library(roxygen2)

# Only need to use create once!
create("Addressr")

# Create folder called manual and
# re-set the working directory to your package folder 
setwd("./addressr")

# Use roxygen2 to auto document your package
document()

# Return the working directory to the parent folder
setwd("C:/Users/engs1/Desktop/Working Documents/ABR_CS1_TradeWaste")

# Install the package
install("addressr")

library(Addressr)
step1(ABR_Dynamic)

# Appendix 6 Extracting levels and landmarks
levelr <- function(x) {
  extract <- data.frame(lapply(x,captable)) %>%
    mutate(t1 = gsub("/ ", "/", t1),
           t1 = word(as.character(AddressLine1), 1)) %>%
    left_join(ABR_Attributes_AL1_Pf,
              by = c("t1")) 
  return(extract)
}

ABR_Levels <- select(ABR_Dynamic, PID, AddressLine1, AddressLine2:Postcode) %>%
  levelr











