#' get_vote
#'
#' Function to get vote count data for a given date from the AP API's elections endpoint
#'
#' @param key Supplied API key
#' @param date Election date supplied in the format of Y-M-D
#' @param state  Optional parameter for state supplied as postal code, e.g. "IA"
#' @param level Level of reporting units. Can take arguments for state, district, fipscode, or reporting unit
#' @param office_id AP ID code for the office position. Optionally can be a list or vector of IDs
#' @param winner If a winners has been declared. Can supply "X" if a races has a declared winer, R if there is a run-off, U if there is no declared winner, or A for all. Defaults to all.
#' @param race_type Type of election. Optionally can be list or vector of types
#' @param party
#' @param test
#' @param national
#' @param uncontested
#'
#' @return Returns a tidy dataframe of election results
#' @export
#'
#' @examples test_ia <- get_vote(key = Sys.getenv("apelex_api_key"), level = "fipscode", state = "IA", date = "2020-02-03")
#' @importFrom httr GET http_type content
#' @importFrom jsonlite fromJSON
#' @importFrom tidyr unnest
#' @importFrom stringr str_detect
#'
#'
get_vote <- function(key = Sys.getenv("apelex_api_key"),
                     date = NULL,
                     state = NULL,
                     level = NULL,
                     office_id = NULL,
                     winner = NULL,
                     race_type = NULL,
                     party = NULL,
                     test = FALSE,
                     national = TRUE,
                     uncontested = FALSE) {


  base_url <- "https://api.ap.org/v2/"

  end_point <- "elections/"

  url_full <- paste0(base_url, end_point, date)


  raw_apelex <- httr::GET(url_full, query = list(

    format = "json",
    level = level,
    statepostal = state,
    apikey = key,
    officeID = office_id,
    winner = winner,
    raceTypeId = race_type,
    test = test,
    party = party,
    national = national,
    uncontested = uncontested

    )

  )

  if (httr::http_type(raw_apelex) != "application/json") {
    stop("Help I'm stuck in a JSON factory: API did not return json", call. = FALSE)
  }


  parsed_apelex <- jsonlite::fromJSON(
    httr::content(raw_apelex, "text"),
    simplifyDataFrame = TRUE
  )

  races_df <- parsed_apelex$races

  tidy_unlist <- function(x) {

    if (!any(stringr::str_detect(sapply(x, class), "list")))

      return(x)

    else

      return(tidy_unlist(tidyr::unnest(x)))

  }

  races_unnested <- tidy_unlist(races_df)

  return(races_unnested)


}
