#' get_vat
#'
#' Function to get vote count data aggregated by strata, geographic region, or party estimates from the AP voter analysis tool
#'
#' @param key Supplied API key
#' @param test TRUE/FALSE. Whether to use test data (TRUE) or production data (FALSE). Defaults to FALSE.
#' @param report The type of VAT report to get data from. Options include: CountyGeoStrata, CountyPartyStrataEstimates, CountyGeoStrataEstimates, CountyPartyStatewideEstimates, CountyGeoStatewideEstimates
#' @param date date of the election in ISO format, e.g. "2020-02-03". Defaults to Sys.Date()
#'
#' @return Returns a tidy dataframe of VAT results
#' @export
#'
#' @importFrom httr GET http_type content
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr filter
#' @importFrom tidyr unnest
#'
#' @examples vat_test <- get_vat(report = "CountyGeoStrata", key = "key = Sys.getenv("apelex_api_key"), date = "2020-02-03)

get_vat <- function(key = Sys.getenv("apelex_api_key"),
                    test = FALSE,
                    report = c("CountyGeoStrata",
                               "CountyPartyStrata",
                               "CountyPartyStrataEstimates",
                               "CountyGeoStrataEstimates",
                               "CountyPartyStatewideEstimates",
                               "CountyGeoStatewideEstimates"),
                    date = Sys.Date()) {

  url <- "https://api.ap.org/v2/reports?type=SPE_VATEstimate"

  vat_reps  <- httr::GET(url, query = list(

    apikey = key,
    format = "json",
    test = test

    )

  )

  parsed_vat_reps <- jsonlite::fromJSON(httr::content(vat_reps, "text"), simplifyDataFrame = TRUE)

  urls_df <- tidyr::unnest(parsed_vat_reps$reports, cols = c(categories))[c(FALSE, TRUE, FALSE, FALSE),]

  urls_date <- dplyr::filter(urls_df, electionDate == !!date)


  urls_switch <- paste0(urls_date$term, " = ", "\"", urls_date$id, "\"", collapse = ",")

  switch <- paste0("switch(", "\"", report, "\"", ",", urls_switch, ")")

  report_new <- eval(parse(text = switch))


  raw_apvat <- httr::GET(report_new, query = list(

    apikey = key,
    test = test

    )

  )

  parsed_vat <- jsonlite::fromJSON(httr::content(raw_apvat, "text"), simplifyDataFrame = TRUE)

  parsed_vat_df <- parsed_vat[[1]]

  return(parsed_vat_df)

}

