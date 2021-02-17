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
#'@param yearFieldPPR Character string. The name of the field in \code{PPR} which contains the Year.
#'@param yearFieldPP Character string. The name of the field in \code{PP} which contains the Year.
#'@param PPField Character string. The name of the field in \code{PP} which contains the PP data.
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


calc_PPR_scaled <- function(PPR,PP,yearFieldPPR="YEAR",PPField = "ANNUAL_MTON",yearFieldPP="YEAR") {

  names(PPR)[names(PPR) == yearFieldPPR] <- "YEAR"
  names(catch)[names(catch) == catchField] <- "catch"
  names(PP)[names(PP) == yearFieldPP] <- "YEAR"
  names(primaryProduction)[names(primaryProduction) == ppField] <- "ANNUAL_MTON"



  joinedTab <- dplyr::left_join(PP,PPR,by = "YEAR")

  scaledIndex <- joinedTab %>%
    dplyr::mutate(SCALEDINDEX = INDEX/ANNUAL_MTON)

  return(scaledIndex)

}
