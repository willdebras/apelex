#' get_vat
#'
#' Function to get vote count data aggregated by strata, geographic region, or party estimates from the AP voter analysis tool
#'
#' @param key Supplied API key
#' @param test
#' @param report The type of VAT report to get data from. Options include: CountyGeoStrata, CountyPartyStrataEstimates, CountyGeoStrataEstimates, CountyPartyStatewideEstimates, CountyGeoStatewideEstimates
#'
#' @return Returns a tidy dataframe of VAT results
#' @export
#'
#' @importFrom httr GET http_type content
#' @importFrom jsonlite fromJSON
#'
#' @examples vat_test <- get_vat(report = "CountyGeoStrata", key = "key = Sys.getenv("apelex_api_key")

get_vat <- function(key = Sys.getenv("apelex_api_key"),
                    test = FALSE,
                    report = c("CountyGeoStrata",
                               "CountyPartyStrata",
                               "CountyPartyStrataEstimates",
                               "CountyGeoStrataEstimates",
                               "CountyPartyStatewideEstimates",
                               "CountyGeoStatewideEstimates")) {

report <- switch(report,
                   CountyGeoStrata = "57ff8fd4e6de4ac59f1e14289654a11d",
                   CountyPartyStrata = "c44163209e0640828fed9eaee3aa7f3a",
                   CountyPartyStrataEstimates = "ab5324f29d1b42d8892287c38bd3ffc7",
                   CountyGeoStrataEstimates = "d55e73de04474622bad451c1617bb6d5",
                   CountyPartyStatewideEstimates = "37cc2caf95094dcc96448095c432dd4a",
                   CountyGeoStatewideEstimates = "ca2e1c70db00439999897230d9023762")

base_url <- "https://api.ap.org/v2/"

end_point <- "reports/"

url_full <- paste0(base_url, end_point, report)

raw_apvat <- httr::GET(url_full, query = list(

  apikey = key

  )

)

parsed_vat <- jsonlite::fromJSON(httr::content(raw_apvat, "text"), simplifyDataFrame = TRUE)

parsed_vat_df <- parsed_vat[[1]]

return(parsed_vat_df)

}
