#' Title
#'
#' @param key
#' @param date
#' @param state
#' @param level
#' @param office_id
#' @param winner
#' @param race_type
#'
#' @return
#' @export
#'
#' @examples
get_vote <- function(key = Sys.getenv("apelex_api_key"),
                     date = NULL,
                     state = NULL,
                     level = NULL,
                     office_id = NULL,
                     winner = NULL,
                     race_type = NULL) {


  base_url <- "https://api.ap.org/v2/elections/"

  end_point <- date

  url_full <- paste0(base_url, end_point)

  raw_apelex <- httr::GET(base_url, query = list(

    date = date,
    format = "json",
    level = level,
    statepostal = state,
    apikey = key,
    officeID = office_id,
    winner = winner,
    raceTypeId = race_type

    )

  )

  parsed_apelex <- jsonlite::fromJSON(
    httr::content(raw_apelex, "text"),
    simplifyDataFrame = TRUE
  )


}
