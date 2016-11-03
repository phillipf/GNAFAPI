#' GNAF Geocode compare to VICMaps
#'
#' This function returns a sample of 1000 addresses from the VICmaps address shapefile and geocodes them using the GNAF API
#' @param VicMAPs SpatialPointsDataFrame of the VicMAPS shapefile
#' @keywords GNAF, VICMaps
#' @export
#' @examples
#' VICMaps()
#' 

VICMaps <- function(spdf = VicMAPS) {
  
  sample <- cbind(spdf@data, spdf@coords) %>%
    mutate(EZI_ADD = gsub("/", " ", EZI_ADD)) %>%
    sample_n(1000) 
  
  sample <- sample %>%
    mutate(PFI = as.numeric(as.character(PFI)))
  
  GNAF_VicMAPs <- GNAFAPI::Geocode(x=sample$coords.x1, y=sample$coords.x2, id=sample$PFI, FULL_ADDRESS=sample$EZI_ADD, radius = 30)
  
  GNAF_VicMAPs2 <- GNAF_VicMAPs %>%
    select(.id,
           addressDetailPid,
           d61AddressNoAlias,
           score) %>%
    mutate(PFI = as.numeric(.id)) %>%
    left_join(select(sample,
                     PFI,
                     EZI_ADD))

}