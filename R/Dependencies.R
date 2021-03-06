#' GNAFAPI package  
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

check_pkg_deps <- function() {
  if(!require(readr)) {
    message("installing the 'readr' package")
    install.packages("readr")
  }
  if(!require(dplyr))
    stop("the 'dplyr' package needs to be installed first")
}