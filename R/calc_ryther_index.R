#' Calculate the Ryther index
#'
#' The Ryther index is composed of total catch presented on a unit area basis for an ecosystem.
#' Theoretical estimates (Link & Watson, 2019), imply the index "should probably be on the order of 0.3 - 1.1 mt km^-2 y^-1 with an extreme limit of 3 mt km^-2 y^-1.
#' Empirically derived tipping points typically occur in fished ecosystems with total catches greater that 3 to 5 mt km^-2 y^-1 with expected system-wide MSYs on the
#'  order of 1 to 3 mt km^-2 y^-1"
#'
#'
#' The units of the index: metric tonnes km^-2 year^-1
#'
#'
#'@param catch Data frame. n x anything. Data containing the total catch in each year. Can be broken down by species. \code{catch} must be in metric tonnes
#'@param area Numeric scalar. Area of the region (km^2) in which catch comes from.
#'@param yearField Character string. The name of the field in \code{catch} which contains the Yearly data.
#'@param catchField Character string. The name of the field in \code{catch} which contains the catch data.
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

calc_ryther_index <- function(catch, area, yearFiled = "YEAR", catchField ="totLand"){

  #rename catch field
  names(catch)[names(catch) == yearField] <- "YEAR"
  names(catch)[names(catch) == catchField] <- "catch"

  ryther <- catch %>% dplyr::group_by(YEAR) %>%
    dplyr::summarise(Index = sum(catch)/area)

  return(ryther)
}
