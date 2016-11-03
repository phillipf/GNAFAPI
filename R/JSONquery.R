#' GNAF Geocode JSON query preparation
#'
#' This function prepares a set of coordinates and address strings to feed into the GNAF geocode function. the result is a list of addresses with their corresponding minimum bounding rectangle for a given radius around a coordinate (x,y).
#' @param x longitude coordinate column in dataframe.
#' y latitude coordinate column in dataframe
#' id the unique identifier columns to be used
#' FULL_ADDRESS the address string for input into the GNAF Geocode function. Full address string including street number (and or unit number. building name etc.), street name, suburb and postcode. NOTE: remove all "/"'s from string for better performance.
#' radius the search radius for the GNAF geocode function to use in meters around the input coordinate.
#' convert are the input coordinates projected in CRS UTMGRS80 zone 55? Defaults to FALSE
#' @keywords GNAF, boundingbox, JSON queary, Geocode
#' @export
#' @examples
#' JSONquery()

JSONquery <- function(x = x, y = y, id = id, FULL_ADDRESS = FULL_ADDRESS, radius, convert = TRUE) {

  if(is.factor(FULL_ADDRESS)) {

    FULL_ADDRESS = as.character(FULL_ADDRESS)

  }

  if(missing(x) & missing(y)) {

    result <- as.list(FULL_ADDRESS)

  }

  else {

    Quarter1Geo <- sp::SpatialPointsDataFrame(coords = data.frame(x = x,
                                                              y = y),
                                          data = data.frame(id),
                                          proj4string = CRS("+proj=utm +zone=55 +south +ellps=GRS80"))

    Quarter1Radius <- rgeos::gBuffer(Quarter1Geo, byid = TRUE, width=radius, joinStyle="ROUND")

    if(convert == TRUE) {

      Quarter1Radius2 <- sp::spTransform(Quarter1Radius, CRS("+proj=longlat +datum=WGS84"))

    }

    else {

      Quarter1Radius2 <- Quarter1Radius

    }

    Quarter1Radius3 <- lapply(Quarter1Radius2@polygons, function(x) sp::bbox(coordinates(x@Polygons[[1]])))

    boundingbox <- lapply(Quarter1Radius3, function(x) list(list(minLat = x[2], minLon = x[1], maxLat = x[4], maxLon = x[3])))

    result <- mapply(c, FULL_ADDRESS, boundingbox, SIMPLIFY = FALSE)

  }

  names(result) <- id

  result
}
