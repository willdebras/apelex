#' Title
#'
#' @param key Supplied API key
#' @param test TRUE/FALSE. Whether to use test data (TRUE) or production data (FALSE). Defaults to FALSE.
#' @param report report id. Can be found at api.ap.org/v2/reports endpoint
#'
#' @return Returns dataframe with report
#' @export
#'
#' @examples
get_report <- function(key = Sys.getenv("apelex_api_key"),
                    test = FALSE,
                    report = NULL) {


  base_url <- "https://api.ap.org/v2/"

  end_point <- "reports/"

  if (is.null(report)) {

    stop("Specify a report.", call. = FALSE)

  }

  url_full <- paste0(base_url, end_point, report)

  raw_report <- httr::GET(url_full, query = list(

    apikey = key

  )

  )

  parsed_report <- jsonlite::fromJSON(httr::content(raw_report, "text"), simplifyDataFrame = TRUE)

  parsed_report_df <- parsed_report[[1]]

  return(parsed_report_df)

}
