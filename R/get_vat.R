#' get_vat
#'
#' Function to get vote count data aggregated by strata, geographic region, or party estimates from the AP voter analysis tool
#'
#' @param key Supplied API key
#' @param test TRUE/FALSE. Whether to use test data (TRUE) or production data (FALSE). Defaults to FALSE.
#' @param report The type of VAT report to get data from. Options include: CountyGeoStrata, CountyPartyStrataEstimates, CountyGeoStrataEstimates, CountyPartyStatewideEstimates, CountyGeoStatewideEstimates
#'
#' @return Returns a tidy dataframe of VAT results
#' @export
#'
#' @importFrom httr GET http_type content
#' @importFrom jsonlite fromJSON
#'
#' @examples vat_test <- get_vat(report = "CountyGeoStrata", key = "key = Sys.getenv("apelex_api_key"))

get_vat <- function(key = Sys.getenv("apelex_api_key"),
                    test = FALSE,
                    report = c("CountyGeoStrata",
                               "CountyPartyStrata",
                               "CountyPartyStrataEstimates",
                               "CountyGeoStrataEstimates",
                               "CountyPartyStatewideEstimates",
                               "CountyGeoStatewideEstimates")) {

report <- switch(report,
                   CountyGeoStrata = "9ce6742096324717bada64791239a351",
                   CountyPartyStrata = "360f8d5addc34ca6b45e7a3d2745bd97",
                   CountyPartyStrataEstimates = "97bf0dbf9fd74784a7b76fbf8c161fde",
                   CountyGeoStrataEstimates = "2ed5c8076dda4bf6ad19d81df68efa40",
                   CountyPartyStatewideEstimates = "a31b5d1e96624d679419b23775f3467d",
                   CountyGeoStatewideEstimates = "fad384546bf145b3b7ece2f31f629bb8")

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
