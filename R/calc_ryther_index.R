#' Calculate the Ryther index
#'
#' The Ryther index is composed of total catch presented on a unit area basis for an ecosystem
#'  using ideas found in Link & Watson (2019). See sources below.
#'
#' Index units: metric tonnes km^-2 year^-1
#'
#'
#'@param catch dataframe. n x anything. Data containg the total catch in each year. Must contain two columns labelled YEAR,catch. Can be listed by species. \code{catch} must be in metric tonnes
#'@param area Numeric scalar. Area of the region (km^2) in which catched come from.
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

#'
#'@family Link Indices
#'
#'@export

## need to generalize column names

calc_ryther_index <- function(catch,area,catchField ="totLand"){

  #rename catch field
  names(catch)[names(catch) == catchField] <- "catch"

  ryther <- catch %>% dplyr::group_by(YEAR) %>%
    dplyr::summarise(Index = sum(catch)/area)

  return(ryther)
}
