#' Calculate the Fogarty index
#'
#' The Fogarty Index is the ratio of total catches to total primary productivity in an ecosystem.
#'
#' Using ideas found in Link & Watson (2019). See sources below
#'
#'
#'@param catch dataframe. n x anything. Data containg the total catch by each species in each year.
#'Columns must be named YEAR,NESPP3,catch
#'@param primaryProduction dataframe. n x anything.
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

calc_fogarty_index <- function(catch,primaryProduction,catchField ="totLand",ppField ="ANNUAL_MEAN"){

  #rename catch field
  names(catch)[names(catch) == catchField] <- "catch"
  names(primaryProduction)[names(primaryProduction)==ppField] <- "ANNUAL_MEAN"

  totCatch <- catch %>%
    dplyr::group_by(YEAR) %>%
    dplyr::summarise(totalCatch = sum(catch)) %>%
    dplyr::left_join(primaryProduction,by="YEAR")

  fogarty <- totCatch %>% dplyr::mutate(Index=totalCatch/ANNUAL_MEAN) %>%
    dplyr::select(YEAR,Index)

  return(fogarty)


}
