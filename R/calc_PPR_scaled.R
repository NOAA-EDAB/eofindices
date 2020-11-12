#' Scale PPR index by toal production
#'
#'
#'
#'@param PPR dataframe (nx2). Primary production required index over time. Two columns YEAR and INDEX
#'@param PP list.
#'
#'@return dataframe.
#'
#'@importFrom magrittr "%>%"
#'
#'@export


calc_PPR_scaled <- function(PPR,PP,units="m") {

  if (tolower(units)=="m") { # convert from km2 to m2 (multiply by 10^6)
    epuAreaM2 <- PP$EPUarea*1000000
  }


  joinedTab <- dplyr::left_join(PP$PP,PPR,by = "YEAR")

  scaledIndex <- joinedTab %>%
    dplyr::mutate(scaled = INDEX/(365*epuAreaM2)) %>%
    dplyr::mutate(SCALEDINDEX = scaled/ANNUAL_MEAN)

  return(scaledIndex)

}
