# Milestone 1: Target specific manufacturing businesses
# Iteration 1: Beverage manufacturing

# Stage 1) GNAF - Quarterly Update
# Last Edit: November 2017

# Functions developed (Approx Line 166):
# One-to-none Description -
onetoone <- function(df) {
  x <- df[duplicated(df$t1.x),] %>%
    filter(NUMBER_FIRST_SUFFIX.y != "" | NUMBER_LAST_SUFFIX.y != "")

  result <- df %>%
    subset.data.frame(!t1.x %in% x$t1.x)
  return(result)
}

# Inputs (Updated Quarterly): ####
# Source Input 1: GNAF ####
# a) View script from H2:
# Sys.setenv(JAVA_HOME='C:\\java-1.8.0-openjdk-1.8.0.111-3\\jre\\')
# install.packages("rJava")
# install.packages("RH2")
# library(RH2)
# con <- DBI::dbConnect( JDBC('org.h2.Driver',
#                        'C:/Users/engs1/.ivy2/cache/com.h2database/h2/jars/h2-1.4.193.jar') ,
#                   'jdbc:h2:~/test', 'gnaf', 'gnaf')

# Read in data from command line H2 export into CSV. Due to absence of Java Virtual Machine (JVM)
# in R to int Scala (sim Java language), the data has been read in .csv for quick delivery of output.
# Script to be updated at later data.
GNAFVerif <- read.csv('N:/ABR/Output 1 Dynamic Model/GNAF/Nov2016_GNAFAddress.txt',
                      sep = ",",
                      stringsAsFactors = FALSE)

# b) Update of Alias Preferences
GNAFALIAS <- read.csv('N:/ABR/GNAF_LocationAddress/NOV16_GNAF+EULA_PipeSeparatedValue/G-NAF/G-NAF NOVEMBER 2016/Standard/VIC_ADDRESS_ALIAS_psv.psv',
                      sep = "|",
                      stringsAsFactors = FALSE)

# Source Input 2: CWW Boundary ####
CWWBoundary <- readOGR("N:/Asset Information/MUNSYS MapInfo Data/Production/Data/CWW Boundary_2014_region.shp",
                       layer="CWW Boundary_2014_region")


# Processing of Data: ####
GNAFVerif$row = 1:nrow(GNAFVerif)

ABR_DSP1 <- SpatialPointsDataFrame(coords = data.frame(x = GNAFVerif$LONGITUDE,
                                                             y = GNAFVerif$LATITUDE,
                                                             stringsAsFactors = FALSE),
                                         data=data.frame(PID = GNAFVerif$row),
                                         proj4string= CRS("+proj=longlat +datum=WGS84")) %>%
  spTransform(CRS(proj4string(CWWBoundary)))


GNAFVerif_Boundary <- GNAFVerif[which(gContains(CWWBoundary, ABR_DSP1, byid = TRUE)), ]
rm(GNAFVerif)

GNAF_Summarised <- GNAFVerif_Boundary %>%
  mutate(FINAL_LEVEL = ifelse(!is.na(LEVEL_NUMBER)&LEVEL_NUMBER_SUFFIX != "",
                              paste0(LEVEL_TYPE," ", LEVEL_NUMBER, LEVEL_NUMBER_SUFFIX),
                              ifelse(!is.na(LEVEL_NUMBER)&LEVEL_NUMBER_SUFFIX == "",
                                     paste0(LEVEL_TYPE, " ", LEVEL_NUMBER),
                                     ifelse(is.na(LEVEL_NUMBER)&!is.na(LEVEL_NUMBER_SUFFIX),
                                            paste0(LEVEL_TYPE, " ", LEVEL_NUMBER_SUFFIX),
                                            LEVEL_TYPE))),
         FINAL_FLAT = ifelse(!is.na(FLAT_NUMBER)&!is.na(FLAT_NUMBER_SUFFIX),
                             paste0(FLAT_NUMBER, FLAT_NUMBER_SUFFIX),
                             ifelse(!is.na(FLAT_NUMBER)&is.na(FLAT_NUMBER_SUFFIX),
                                    FLAT_NUMBER,
                                    ifelse(is.na(FLAT_NUMBER)&!is.na(FLAT_NUMBER_SUFFIX),
                                           FLAT_NUMBER_SUFFIX,
                                           NA))),
         FINAL_HOUSE_NUMBER =  ifelse(!is.na(NUMBER_LAST),
                                      paste0(NUMBER_FIRST, "-", NUMBER_LAST),
                                      NUMBER_FIRST),
         FINAL_HOUSE_NUMBER = ifelse(NUMBER_FIRST_SUFFIX != "",
                                     paste0(FINAL_HOUSE_NUMBER, NUMBER_FIRST_SUFFIX),
                                     FINAL_HOUSE_NUMBER),
         FINAL_HOUSE_NUMBER = ifelse(NUMBER_FIRST_PREFIX != "",
                                     paste0(NUMBER_FIRST_PREFIX, FINAL_HOUSE_NUMBER),
                                     FINAL_HOUSE_NUMBER),
         FINAL_HOUSE_NUMBER = ifelse(NUMBER_LAST_SUFFIX != "",
                                     paste0(FINAL_HOUSE_NUMBER, NUMBER_LAST_SUFFIX),
                                     FINAL_HOUSE_NUMBER),
         FINAL_HOUSE_NUMBER = ifelse(!is.na(LOT_NUMBER),
                                     paste("LOT", LOT_NUMBER),
                                     FINAL_HOUSE_NUMBER),
         STREET_STRING = paste(STREET_NAME, STREET_TYPE_CODE),
         STREETDIRECTIONAL = ifelse(STREET_SUFFIX_TYPE != "",
                                    paste(STREET_STRING, STREET_SUFFIX_TYPE),
                                    STREET_STRING)) %>%
  mutate(Descriptor = ifelse(FINAL_FLAT == ""&FINAL_LEVEL == " "&FINAL_HOUSE_NUMBER != "",
                             "PROPERTY",
                             "TENANT"),
         Descriptor = ifelse(Descriptor == "PROPERTY"&!is.na(LOT_NUMBER),
                             "LOT",
                             Descriptor)) %>%
  mutate(H_Order = ifelse(Descriptor == "PROPERTY"|Descriptor == "LOT",
                          "BASE",
                          ifelse((Descriptor == "TENANT")&FINAL_LEVEL == " ",
                                 "UNIT",
                                 "LEVEL")),
         H_Order = ifelse(H_Order == "LEVEL"&FINAL_FLAT == "",
                          "LEVEL_ONLY",
                          H_Order))

GNAF_Catalogue <- select(GNAF_Summarised,
                         ADDRESS_DETAIL_PID,
                         LATITUDE, LONGITUDE,
                         FINAL_LEVEL, FLAT_TYPE, FINAL_FLAT:STREETDIRECTIONAL,
                         LOCALITY_NAME, POSTCODE,
                         STREET_LOCALITY_PID, LOCALITY_PID,
                         CONFIDENCE:PRIMARY_SECONDARY,
                         Descriptor:H_Order) %>%
  group_by(FINAL_HOUSE_NUMBER, STREETDIRECTIONAL, LOCALITY_NAME, POSTCODE, Descriptor) %>%
  summarise(n = n()) %>%
  spread(Descriptor, n)

# Note: Base and tenant list won't add up because it does not include Lots
GNAF_Preference_Base <- select(GNAF_Summarised,
                               ADDRESS_DETAIL_PID,
                               LATITUDE, LONGITUDE,
                               NUMBER_FIRST, NUMBER_LAST,  NUMBER_FIRST_SUFFIX, NUMBER_LAST_SUFFIX,
                               FINAL_LEVEL, FLAT_TYPE, FINAL_FLAT:STREETDIRECTIONAL,
                               LOCALITY_NAME, POSTCODE,
                               STREET_LOCALITY_PID, LOCALITY_PID,
                               CONFIDENCE:PRIMARY_SECONDARY,
                               Descriptor:H_Order) %>%
  filter(Descriptor == "PROPERTY")

GNAFALIAS_Base <- GNAFALIAS %>%
  subset.data.frame(ALIAS_PID %in% GNAF_Preference_Base$ADDRESS_DETAIL_PID)

GNAF_Preference_Base <- GNAF_Preference_Base %>%
  left_join(select(GNAFALIAS_Base, ALIAS_PID, PRINCIPAL_PID),
            by = c("ADDRESS_DETAIL_PID" = "ALIAS_PID")) %>%
  mutate(Return_GAVICPROP = ifelse(is.na(PRINCIPAL_PID),
                                   ADDRESS_DETAIL_PID,
                                   PRINCIPAL_PID),
         t1 = paste(FINAL_HOUSE_NUMBER, STREETDIRECTIONAL, LOCALITY_NAME, POSTCODE))

GNAF_Preference_Tenant <- select(GNAF_Summarised,
                                 ADDRESS_DETAIL_PID,
                                 LATITUDE, LONGITUDE,
                                 NUMBER_FIRST, NUMBER_LAST, NUMBER_FIRST_SUFFIX, NUMBER_LAST_SUFFIX,
                                 FINAL_LEVEL, FLAT_TYPE, FINAL_FLAT:STREETDIRECTIONAL,
                                 LOCALITY_NAME, POSTCODE,
                                 STREET_LOCALITY_PID, LOCALITY_PID,
                                 CONFIDENCE:PRIMARY_SECONDARY,
                                 Descriptor:H_Order) %>%
  filter(Descriptor == "TENANT")

GNAFALIAS_Ten <- GNAFALIAS %>%
  subset.data.frame(ALIAS_PID %in% GNAF_Preference_Tenant$ADDRESS_DETAIL_PID)

GNAF_Preference_Tenant <- GNAF_Preference_Tenant %>%
  left_join(select(GNAFALIAS_Ten, ALIAS_PID, PRINCIPAL_PID),
            by = c("ADDRESS_DETAIL_PID" = "ALIAS_PID")) %>%
  mutate(Return_GAVICTEN = ifelse(is.na(PRINCIPAL_PID),
                                  ADDRESS_DETAIL_PID,
                                  PRINCIPAL_PID),
         t1 = paste(FINAL_HOUSE_NUMBER, STREETDIRECTIONAL, LOCALITY_NAME, POSTCODE)) %>%
  left_join(select(GNAF_Preference_Base, t1, Return_GAVICPROP),
            by = c("t1"))

# Broken Alias
GNAF_Broken <- GNAF_Preference_Tenant %>%
  filter(is.na(Return_GAVICPROP)) %>%
  select(t1, NUMBER_FIRST, NUMBER_LAST, NUMBER_FIRST_SUFFIX, NUMBER_LAST_SUFFIX, STREET_LOCALITY_PID, LOCALITY_PID) %>%
  unique()

GNAF_Kin1 <- GNAF_Broken %>%
  inner_join(GNAF_Preference_Base, by=c("NUMBER_FIRST", "STREET_LOCALITY_PID", "LOCALITY_PID"))

onetoone <- function(df) {
  x <- df[duplicated(df$t1.x),] %>%
    filter(NUMBER_FIRST_SUFFIX.y != "" | NUMBER_LAST_SUFFIX.y != "")

  result <- df %>%
    subset.data.frame(!t1.x %in% x$t1.x)
  return(result)
}

library(plyr)
GNAF_Kin1 <- ddply(GNAF_Kin1,.(t1.x), function(x) onetoone(x))
GNAF_Kin1 <- ddply(GNAF_Kin1,.(t1.x),function(x) tail(x,1))
#GNAF_Kin1[duplicated(GNAF_Kin1$t1.x),]
detach("package:plyr", unload=TRUE)

# Investigative/exploratory code
GNAF_test11 <- GNAF_Kin1 %>%
  group_by(t1.x) %>%
  summarise(n = n())

GNAF_Kin2 <- GNAF_Broken %>%
  left_join(GNAF_Preference_Base, by=c("NUMBER_FIRST", "STREET_LOCALITY_PID", "LOCALITY_PID")) %>%
  filter(is.na(Return_GAVICPROP)) %>% select(1:7) %>%
  left_join(GNAF_Preference_Base, by=c("STREET_LOCALITY_PID", "LOCALITY_PID")) %>%
  filter(NUMBER_FIRST.x == NUMBER_LAST)
colnames(GNAF_Kin2)[30] <- c("t1.y")

library(plyr)
GNAF_Kin2 <- ddply(GNAF_Kin2,.(t1.x), function(x) onetoone(x))
GNAF_Kin2 <- ddply(GNAF_Kin2,.(t1.x),function(x) tail(x,1))
detach("package:plyr", unload=TRUE)

# Investigative/exploratory code
GNAF_test22 <- GNAF_Kin2 %>%
  group_by(t1.x) %>%
  summarise(n = n())

GNAF_Kinship <- rbind(select(GNAF_Kin1, t1.x, t1.y, CONFIDENCE, Return_GAVICPROP),
                      select(GNAF_Kin2, t1.x, t1.y, CONFIDENCE, Return_GAVICPROP))
colnames(GNAF_Kinship)[1:2] <- c("BrokenBase", "CorrectedPrimaryBase")

GNAF_Kin3 <- select(GNAF_Broken, t1) %>%
  subset.data.frame(!t1 %in% GNAF_Kinship$BrokenBase) %>%
  mutate(t1.x = t1,
         t1.y = "NonExistent",
         CONFIDENCE = "NonExistent",
         Return_GAVICPROP = "NonExistent")

GNAF_Kinship <- rbind(select(GNAF_Kin1, t1.x, t1.y, CONFIDENCE, Return_GAVICPROP),
                      select(GNAF_Kin2, t1.x, t1.y, CONFIDENCE, Return_GAVICPROP),
                      select(GNAF_Kin3, t1.x, t1.y, CONFIDENCE, Return_GAVICPROP))
colnames(GNAF_Kinship)[1:2] <- c("BrokenBase", "CorrectedPrimaryBase")

rm(GNAF_Kin1)
rm(GNAF_Kin2)
rm(GNAF_Kin3)
#rm(GNAF_Kinship)

# Update on preferences
GNAF_Preference_Tenant1 <- GNAF_Preference_Tenant %>%
  filter(is.na(Return_GAVICPROP)) %>% select(-Return_GAVICPROP) %>%
  left_join(select(GNAF_Kinship, BrokenBase, Return_GAVICPROP),
            by = c("t1" = "BrokenBase"))

GNAF_Preference_Tenant2 <- GNAF_Preference_Tenant %>%
  filter(!is.na(Return_GAVICPROP))

GNAF_Preference_Tenant3 <- rbind(GNAF_Preference_Tenant2, GNAF_Preference_Tenant1)
rm(GNAF_Preference_Tenant1)
rm(GNAF_Preference_Tenant2)
rm(GNAF_Preference_Tenant)

GNAF_Preference_Lot <- GNAF_Summarised %>%
  select(ADDRESS_DETAIL_PID,
         LATITUDE, LONGITUDE,
         NUMBER_FIRST, NUMBER_LAST,  NUMBER_FIRST_SUFFIX, NUMBER_LAST_SUFFIX,
         FINAL_LEVEL, FLAT_TYPE, FINAL_FLAT:STREETDIRECTIONAL,
         LOCALITY_NAME, POSTCODE,
         STREET_LOCALITY_PID, LOCALITY_PID,
         CONFIDENCE:PRIMARY_SECONDARY,
         Descriptor:H_Order) %>%
  filter(Descriptor == "LOT") %>% mutate(Return_GAVICPROP = NA, Return_GAVICTEN = NA)

GNAF_MAP <- select(GNAF_Preference_Base, ADDRESS_DETAIL_PID:H_Order, Return_GAVICPROP) %>%
  mutate(Return_GAVICTEN = NA) %>%
  rbind(select(GNAF_Preference_Tenant3, ADDRESS_DETAIL_PID:H_Order, Return_GAVICPROP, Return_GAVICTEN),
        select(GNAF_Preference_Lot, ADDRESS_DETAIL_PID:H_Order, Return_GAVICPROP, Return_GAVICTEN))

Return_Property <- select(GNAF_Preference_Base, Return_GAVICPROP) %>% unique() %>%
  left_join(select(GNAF_Preference_Base, ADDRESS_DETAIL_PID, FINAL_LEVEL:STREETDIRECTIONAL, LOCALITY_NAME, POSTCODE),
            by = c("Return_GAVICPROP" = "ADDRESS_DETAIL_PID"))
colnames(Return_Property)[2:9] <- c("PP_Level", "PP_FlatType", "PP_FlatNumber","PP_HouseNumber",
                                    "PP_StreetStr","PP_StreetDirectional", "PP_LocalityName", "PP_Postcode")

Return_TenantProperties <- select(GNAF_Preference_Tenant3, Return_GAVICPROP) %>% unique() %>%
  left_join(select(GNAF_Preference_Tenant3, ADDRESS_DETAIL_PID, FINAL_LEVEL:STREETDIRECTIONAL, LOCALITY_NAME, POSTCODE),
            by = c("Return_GAVICPROP" = "ADDRESS_DETAIL_PID"))
colnames(Return_TenantProperties)[2:9] <- c("PP_Level", "PP_FlatType", "PP_FlatNumber","PP_HouseNumber",
                                            "PP_StreetStr","PP_StreetDirectional", "PP_LocalityName", "PP_Postcode")

Return_PropFull <- rbind(Return_Property, Return_TenantProperties) %>% unique() %>%
  filter(!is.na(PP_StreetDirectional))
#test<- Return_PropFull[!duplicated(Return_PropFull$Return_GAVICPROP),]
colnames(Return_PropFull)[2:9] <- c("PP_Level", "PP_FlatType", "PP_FlatNumber","PP_HouseNumber",
                                    "PP_StreetStr","PP_StreetDirectional", "PP_LocalityName", "PP_Postcode")

Return_Tenant <- select(GNAF_Preference_Tenant3, Return_GAVICTEN) %>% unique() %>%
  left_join(select(GNAF_Preference_Tenant3, ADDRESS_DETAIL_PID, FINAL_LEVEL:STREETDIRECTIONAL, LOCALITY_NAME, POSTCODE),
            by = c("Return_GAVICTEN" = "ADDRESS_DETAIL_PID"))
colnames(Return_Tenant)[2:9] <- c("PT_Level", "PT_FlatType", "PT_FlatNumber","PT_HouseNumber",
                                  "PT_StreetStr","PT_StreetDirectional", "PT_LocalityName", "PT_Postcode")
test<- Return_Tenant[duplicated(Return_Tenant$Return_GAVICTEN),]

GNAF_MAP2 <- GNAF_MAP %>%
  left_join(Return_PropFull,
            by = c("Return_GAVICPROP")) %>%
  left_join(Return_Tenant,
            by = c("Return_GAVICTEN"))
rm(GNAF_MAP)
write.table(GNAF_MAP2, "GNAF_MAP.csv", sep = ",", row.names = FALSE)

GNAF_Summarised1 <- select(GNAF_Summarised, ADDRESS_DETAIL_PID, row,
                           FINAL_LEVEL:STREETDIRECTIONAL, LOCALITY_NAME, POSTCODE, Descriptor, H_Order) %>%
  group_by(FINAL_HOUSE_NUMBER, STREETDIRECTIONAL, LOCALITY_NAME, POSTCODE, Descriptor, H_Order) %>%
  summarise(n = n()) %>%
  spread(Descriptor, n)

write.table(GNAF_Summarised, "GNAF_Nov_GNAFPID.csv", sep = ",", row.names = FALSE)
write.table(GNAF_Summarised1, "GNAF_Nov_Summarised.csv", sep = ",", row.names = FALSE)
