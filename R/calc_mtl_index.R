#' Calculate the mean Trophic Level
#'
#' mean Trophic Level is calculated from a time series of landings: It is the sum of the species Trophic levels #' weighted by their landings
#'#'
#'
#'@param data dataframe. n x anything. Data containing the total catch by each species in each year. A complete grid of year by species must be passed. Columns must be named YEAR,NESPP3,catch.
#'@param speciesTL dataframe. m x anything. Species codes and trophic level for the most abundantly caught species. One column must be named NESPP3 and another Troph
#'@param speciesCode Character string. The name of the column that holds the speciesCodes. Default = "NESPP3".
#'
#'@return Data frame:
#'
#'\item{Year}{Year of the index}
#'\item{Index}{Value of the index}
#'
#'
#'
#'@export

## need to generalize column names

calc_mtl_index <- function(catch,speciesTL,speciesCode="NESPP3"){

  #preallocate dataframe
  years <- unique(catch$YEAR)
  meanTL <- as.data.frame(matrix(data=NA,nrow=length(years),ncol=2))
  names(meanTL) <- c("Year","Index")

  # for each year calculate the mean trophic level
  for (iy in 1:length(years)) {
    # select the current year
    data <- catch %>% dplyr::filter(YEAR == years[iy])
    # join catch with species(this contains trophic level)
    master <- dplyr::left_join(data,speciesTL,by=speciesCode)

    # calulate index
    meanTL$Year[iy] <- years[iy]
    # decision rule for missing data. ALL to zero
    master$totLand[is.na(master$totLand)] <- 0
    # index
    meanTL$Index[iy] <- sum(master$totLand * master$Troph) / sum(master$totLand)
  }

  return(meanTL)
}
