#' GNAF geocode
#'
#' This function communicates with the GNAF geocoding API to return an official address.
#' @param x longitude coordinate column in dataframe.
#' y latitude coordinate column in dataframe
#' id the unique identifier columns to be used
#' FULL_ADDRESS the address string for input into the GNAF Geocode function. Full address string including street number (and or unit number. building name etc.), street name, suburb and postcode. NOTE: remove all "/"'s from string for better performance.
#' radius the search radius for the GNAF geocode function to use in meters around the input coordinate.
#' convert are the input coordinates projected in CRS UTMGRS80 zone 55? Defaults to FALSE
#' @keywords GNAF, Geocode, fuzzymatching, Address
#' @export
#' @examples
#' Geocode()
#'

Geocode <- function(x, y, id, FULL_ADDRESS, radius, fuzzy = FALSE, convert = TRUE) {

  if(!require(sp)) {
    message("installing the 'sp' package")
    install.packages("sp")
  }

  if(!require(rgeos)) {
    message("installing the 'rgeos' package")
    install.packages("rgeos")
  }

  if(!require(rgdal)) {
    message("installing the 'rgdal' package")
    install.packages("rgdal")
  }

  if(!require(shotGroups)) {

    if(!require(devtools)) {
      message("installing the 'devtools' package")
      install.packages("devtools")
    }

    message("installing the 'shotGroups' package")
    packageurl <- "https://cran.r-project.org/bin/windows/contrib/3.3/CompQuadForm_1.4.2.zip"
    install.packages(packageurl, repos=NULL, type="source")
    pkgs <- c('dwoll/shotGroups')

    devtools::install_github(pkgs)
  }

  if(!require(jsonlite)) {
    message("installing the 'jsonlite' package")
    install.packages("jsonlite")
  }

  if(!require(httr)) {
    message("installing the 'httr' package")
    install.packages("httr")
  }

  if(!require(plyr)) {
    message("installing the 'plyr' package")
    install.packages("plyr")
  }

  Query <- GNAFAPI::JSONquery(x = x, y = y, id = id,  FULL_ADDRESS = FULL_ADDRESS, radius = radius, convert = convert)

  if(missing(x) & missing(y)) {

    GNAF <- lapply(Query, function(x) GNAFAPI::Request(x, fuzzy = fuzzy))

  }

  else {
  GNAF <- lapply(Query, function(x) GNAFAPI::Request(Address = x[[1]], boundingbox = x[[2]]))

  }

  GNAF2 <- lapply(GNAF, GNAFAPI::Process)

  GNAF3 <- GNAF2[!is.na(GNAF2)]

  GNAF4 <- ldply(GNAF3)

}
