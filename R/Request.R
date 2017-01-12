#' GNAF geocode request
#'
#' This function communicates with the GNAF geocoding API to return an official address.
#' @param Address Full address string including street number (and or unit number. building name etc.), street name, suburb and postcode.
#' boundingbox a named list of numeric coordinates in WGS 84 projection marking the minimum bounding rectangle of the search area. The
#' list should be in the format: minLat, minLon, maxLat, maxLon.
#' fuzzy puts boundaries around string matching parameters I.e. MaxEdits, MinLength and prefixLength. Defaults to FALSE
#' @keywords GNAF, Geocode, fuzzymatching, Address
#' @export
#' @examples
#' Request()

Request <- function(Address, boundingbox, fuzzy = FALSE) {

  if(missing(boundingbox) & fuzzy == FALSE) {
    body <- list(addr = Address,
                 numHits = 1
                 )
  }

  else if(missing(boundingbox) & fuzzy == TRUE) {
    body <- list(addr = Address,
                 numHits = 1,
                 fuzzy =
                 list(maxEdits = 2,
                 minLength = 4,
                 prefixLength = 0
                 )
                 )
  }

  else if(!missing(boundingbox) & fuzzy == FALSE) {
    body <- list(addr = Address,
                 numHits = 1,
                 box = boundingbox
    )
  }

  else {
    body <- list(addr = Address,
                 numHits = 1,
                 fuzzy =
                 list(maxEdits = 2,
                 minLength = 4,
                 prefixLength = 0
                 ),
                 box = boundingbox
                )
  }

  req <- httr::POST("http://localhost:9040/search",
              body = body, encode = "json")

  response <- httr::content(req)

  if(response$totalHits == 0) {

    body <- list(addr = Address,
                 numHits = 1
    )

    req <- httr::POST("http://localhost:9040/search",
                body = body, encode = "json")
  }

  req
}
