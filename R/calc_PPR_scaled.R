#' Scale PPR index by total primary production
#'
#' Total primary production (PP) has units gC m^-1 d^-1. catch has units gC year^-1.
#' Catch is converted to same units as PP.
#' The ratio of PPR:PP is then returned
#'
#' Units: Percentage
#'
#'
#'@param PPR Data frame (n x 2). Primary production required index over time. Two columns YEAR and INDEX
#'@param PP List. First element labeled \code{PP} is a Data frame of size (n x 2). Columns YEAR, ANNUAL_MEAN. Second element labeled \code{EPUarea} is the area of the region
#'@param units Character. Describes the units of \code{PP$EPUarea}
#'@param col Character String. Name of column holding the PP data used to scale
#'
#'
#'@return Data frame (n x 7)
#'\item{YEAR}{Year of scaled index}
#'\item{scaled}{PPR index scaled to same units as PP}
#'\item{SCALEDINDEX}{Ratio of PPR:PP}
#'
#'@importFrom magrittr "%>%"
#'
#'@export


calc_PPR_scaled <- function(PPR,PP,units="m",col="Constant") {

  if (tolower(units)=="m") { # convert from km2 to m2 (multiply by 10^6)
    epuAreaM2 <- PP$EPUarea*1000000
  }

  PrimProd <- PP[[col]]

  joinedTab <- dplyr::left_join(PrimProd,PPR,by = "YEAR")

  scaledIndex <- joinedTab %>%
    dplyr::mutate(scaled = INDEX/(365*epuAreaM2)) %>%
    dplyr::mutate(SCALEDINDEX = scaled/ANNUAL_MEAN)

  return(scaledIndex)

}
