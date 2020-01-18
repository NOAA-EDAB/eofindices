#' Calculate the PPR index
#'
#' Using ideas found in Pauly & Christensen's (1995) Nature publication,
#' Primary Production Required is calculated as:
#'
#'
#'
#'@param catch dataframe. n x anything. Data containg the total catch by each species in each year.
#'Columns must be named YEAR,NESPP3,catch
#'@param speciesTL dataframe. m x anything. Species codes and trophic level for the most abundantly caught species.
#'One column must be named NESPP3 and another Troph
#'@param speciesCode Character string. The name of the column that holds the speciesCodes. Default = "NESPP3".
#'
#'@return Data frame:
#'
#'\item{Year}{Year of the index}
#'\item{Index}{Value of the index}
#'
#'
#'@export

## need to generalize column names

calc_ppr_index <- function(catch,speciesTL,speciesCode = "NESPP3"){

  #preallocate dataframe
  years <- unique(catch$YEAR)
  PPR <- as.data.frame(matrix(data=NA,nrow=length(years),ncol=2))
  names(PPR) <- c("Year","Index")

  # for each year calculate the PPR score
  for (iy in 1:length(years)) {
    # select the current year
    data <- catch %>% dplyr::filter(YEAR == years[iy])
    # join catch with species(this contains trophic level)
    master <- dplyr::left_join(data,speciesTL,by=speciesCode)

    # calulate index
    PPR$Year[iy] <- years[iy]
    # decision rule for missing data. ALL to zero
    master$totLand[is.na(master$totLand)] <- 0
    # index
    PPR$Index[iy] <- sum((master$totLand/9)*10^(master$Troph-1))
  }

  return(PPR)
}
