#' Process the JSON result from the GNAF Geocode function
#'
#' This function takes the resulting list of JSON responses from the GNAF Geocode function and returns a list of results.
#' @param responses list of JSON responeses from the GNAF Geocode function.
#' @keywords GNAF, JSON, GNAF Geocode
#' @export
#' @examples
#' Process()
#'

Process <- function(response) {

  response <- content(response)

  if(response$totalHits == 0) {

    return(NA)
  }

  else {

    response2 <- jsonlite::fromJSON(response$hits[[1]]$json)
    response3 <- response$hits[[1]]

    result = cbind(data.frame(lapply(unlist(response2), type.convert),
                              stringsAsFactors=FALSE),
                   d61Address = response3$d61Address[[1]],
                   d61AddressNoAlias = response3$d61AddressNoAlias,
                   score = response3$score)

    return(result)
  }
}
