
CWW_DW <- odbcDriverConnect(connection="Driver={SQL Server};
                            server=wvdb1devsql;
                            database=ABR;
                            trusted_connection=true")

CWWBoundary <- readOGR("N:/Asset Information/MUNSYS MapInfo Data/Production/Data/CWW Boundary_2014_region.shp",
                       layer="CWW Boundary_2014_region")
b <- CWWBoundary %>%
     spTransform(CRS("+proj=longlat +datum=WGS84"))

CWW_BOUNDARY <- data.frame(b@polygons[[1]]@Polygons[[1]]@coords)

colnames(CWW_BOUNDARY) <- c("X", "Y")

sqlSave(CWW_DW, CWW_BOUNDARY, rownames = FALSE, colnames = FALSE, verbose = FALSE,
        safer = TRUE, addPK = FALSE, 
        fast = TRUE, test = FALSE, nastring = NULL)