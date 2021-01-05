#' Calculate the Fogarty index
#'
#' The Fogarty Index is the ratio of total catches to total primary productivity in an ecosystem.
#' Link and Watson (2019) state ".. Fogarty ration of 0.22 to  0.92 per mil, with an extreme limit of ~2.5 emerges from theoretically based limits coupled with estimates of global catches"
#'
#' The units of the index: unitless
#'
#'
#'@param catch Data frame. n x anything. Data containing the total catch (g year^-1) by each species in each year.
#'@param primaryProduction Data frame. n x anything. (gC m^-2 d^-1)
#'@param yearField Character string. The name of the field in \code{catch} which contains the Yearly data.
#'@param catchField Character string. The name of the field in \code{catch} which contains the catch data.
#'@param ppField Character string. The name of the field in \code{primaryProduction} which contains the ANNUAL_MEAN data.
#'
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

calc_fogarty_index <- function(catch, primaryProduction, yearField="YEAR", catchField ="totLand", ppField ="ANNUAL_MEAN"){

  #rename catch field
  names(catch)[names(catch) == yearField] <- "YEAR"
  names(catch)[names(catch) == catchField] <- "catch"
  names(primaryProduction)[names(primaryProduction)==ppField] <- "ANNUAL_MEAN"

  # convert gC m-2 d-1 to gC year-1 for the EPU
  totPP <- primaryProduction$PP %>%
    dplyr::mutate(totalPP = ANNUAL_MEAN * (1000^2) * primaryProduction$EPUarea * 365)

  # divide by 9 to go from wet weight to Carbon
  totCatch <- catch %>%
    dplyr::group_by(YEAR) %>%
    dplyr::summarise(totalCatch = sum(catch)/9) %>%
    dplyr::left_join(totPP,by="YEAR")

  fogarty <- totCatch %>% dplyr::mutate(Index=totalCatch/totalPP) %>%
    dplyr::select(YEAR,Index)

  return(fogarty)


}
