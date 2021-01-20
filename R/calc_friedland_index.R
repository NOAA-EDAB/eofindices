#' Calculate the Friedland index
#'
#' The Friedland index is the ratio of total catches to chlorophyll in an ecosystem.
#' Link & Watson (2019), "... propose a threshold of ~1 to delineate Ecosystem overfishing"
#'
#' The units of the index: unitless
#'
#'@param catch Data Frame. n x anything. Data containing the total catch by each species in each year.
#'@param chlorophyll Data frame. n x anything. Same units as catch
#'@param yearField Character string. The name of the field in \code{catch} which contains the Yearly data.
#'@param catchField Character string. The name of the field in \code{catch} which contains the catch data.
#'@param chloroField Character string. The name of the field in \code{chlorophyll} which contains the ANNUAL_MEAN data.
#'
#'@return Data frame:
#'
#'\item{YEAR}{Year of the index}
#'\item{INDEX}{Value of the index}
#'
#'@section Sources:
#'
#'J. S. Link, R. A. Watson, Global ecosystem overfishing: Clear delineation within real limits to production. Sci. Adv. 5, eaav0474 (2019). \url{DOI: 10.1126/sciadv.aav0474}
#'
#'@importFrom magrittr "%>%"
#'
#'@family Link Indices
#'
#'@export

## need to generalize column names

calc_friedland_index <- function(catch, chlorophyll, yearField="YEAR", catchField ="totLand",chloroField ="ANNUAL_MEAN"){

  #rename catch field
  names(catch)[names(catch) == yearField] <- "YEAR"
  names(catch)[names(catch) == catchField] <- "catch"
  names(chlorophyll)[names(chlorophyll)==chloroField] <- "ANNUAL_MEAN"

  totCatch <- catch %>%
    dplyr::group_by(YEAR) %>%
    dplyr::summarise(totalCatch = sum(catch)) %>%
    dplyr::left_join(chlorophyll,by="YEAR")

  friedland <- totCatch %>% dplyr::mutate(Index=totalCatch/ANNUAL_MEAN) %>%
    dplyr::select(YEAR,Index)

  return(friedland)


}
