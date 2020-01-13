#' Calculate the mean Trophic Level
#'
#' mean Trophic Level is calculated from a time series of landings
#'
#'
#'@param data dataframe. n x anything. Data containg the total catch by each species in each year.
#'Columns must be named YEAR,NESPP3,catch
#'@param speciesTL dataframe. m x anything. Species codes and trophic level for the most abundantly caught species.
#'One column must be named NESPP3 and another Troph
#'
#'@return
#'
#'
#'@export

## need to generalize column names

mean_trophic_level <- function(catch,speciesTL){

  #preallocate dataframe
  years <- unique(catch$YEAR)
  meanTL <- as.data.frame(matrix(data=NA,nrow=length(years),ncol=2))
  names(meanTL) <- c("Year","MTL")

  # for each year calculate the mean trophic level
  for (iy in 1:length(years)) {
    # select the current year
    data <- catch %>% dplyr::filter(YEAR == years[iy])
    # join catch with species(this contains trophic level)
    master <- dplyr::left_join(data,speciesTL,by="NESPP3")

    # calulate index
    meanTL$Year[iy] <- years[iy]
    # decision rule for missing data. ALL to zero
    master$totLand[is.na(master$totLand)] <- 0
    # index
    meanTL$MTL[iy] <- sum(master$totLand * master$Troph) / sum(master$totLand)
  }

  return(meanTL)
}
