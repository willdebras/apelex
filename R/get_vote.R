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
    raceTypeId = race_type

    )

  )

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
