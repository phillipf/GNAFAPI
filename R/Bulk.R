#' GNAF bulk request
#'
#' This function communicates with the GNAF geocoding API to return an official address.
#' @param Addresses Full address string including street number (and or unit number. building name etc.), street name, suburb and postcode.
#' boundingbox a named list of numeric coordinates in WGS 84 projection marking the minimum bounding rectangle of the search area. The
#' list should be in the format: minLat, minLon, maxLat, maxLon.
#' fuzzy puts boundaries around string matching parameters I.e. MaxEdits, MinLength and prefixLength. Defaults to FALSE
#' @keywords GNAF, Geocode, fuzzymatching, Address
#' @export
#' @examples
#' Bulk()

Bulk <- function(df, boundingbox, fuzzy = FALSE) {

    #Addresses = paste0("[", paste0(df[["ADDRESS_STRING"]], collapse = ", "), "]")
    Addresses = df[["ADDRESS_STRING"]]

    names(Addresses) <- df[["AG_CONSUMERNUMBER"]]

    Addresses = split(Addresses, ceiling(seq_along(Addresses)/50))

    #Addresses = as.list(Addresses)

    Result <- list()

    Process <- function(response) {



        #response2 <- jsonlite::fromJSON(response$hits[[1]]$json)
        response3 <- response$hits[[1]]
        response4 <- jsonlite::fromJSON(response3$json)
        #result = cbind(data.frame(lapply(unlist(response2), type.convert),
                                  #stringsAsFactors=FALSE),
                       #d61Address = response3$d61Address[[1]],
                       #d61AddressNoAlias = response3$d61AddressNoAlias,
                       #score = response3$score)
        response3$json <- NULL

        result = c(response3, response4)

        return(result)

    }

    search <- function(chunk) {

      body <- list(#addresses = as.list(unname(chunk)),
                  addresses = as.list(unname(chunk)),
                   numHits = 1,
                   fuzzy =
                     list(maxEdits = 2,
                          minLength = 4,
                          prefixLength = 0
                     )
      )


      req <- httr::POST("http://localhost:9040/bulkSearch",
                      body = body, encode = "json")

      #response <- Process(req)
      response <- httr::content(req)

      response <- lapply(response, Process)

      names(response) <- names(chunk)

      .GlobalEnv$Result <- c(.GlobalEnv$Result, response)

      #Result <- c(Result, response)

    }

    numMatch <- function(chunk) {




    }

    lapply(Addresses, search)

    #Result2 <- purrr::flatten(.GlobalEnv$Result)

    #return(Result2)

    # purrr::map2(Result, df[["textbox46"]])




}
