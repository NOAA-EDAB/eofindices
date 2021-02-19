#' Calculate the PPR index
#'
#' Using ideas found in Pauly & Christensen's (1995) Nature publication,
#' Primary Production Required is calculated as:
#'
#' Units: gC year^-1
#'
#'@param catch Data frame. n x m Data containing the total catch (g) by each species in each year.
#'Must include two columns named YEAR,catch, a third column must contain the NESPP3 codes
#'@param speciesTL Data frame. m x anything. Species codes and trophic level for the most abundantly caught species.
#'One column must be named NESPP3 and another Troph
#'@param transferEfficiency Numeric scalar. The value of transfer efficiency to use in the PPR calculation. Range should be [0,1] (Default = 0.10)
#'@param speciesCode Character string. The name of the column that holds the species codes. Default = "NESPP3".
#'
#'@return Data frame:
#'
#'\item{YEAR}{Year of the index}
#'\item{INDEX}{Value of the index}
#'
#'@importFrom magrittr "%>%"
#'
#'@export

## need to generalize column names

calc_ppr_index <- function(catch,speciesTL,transferEfficiency=0.1,speciesCode = "NESPP3"){

  #preallocate dataframe
  years <- unique(catch$YEAR)
  PPR <- as.data.frame(matrix(data=NA,nrow=length(years),ncol=2))
  names(PPR) <- c("YEAR","INDEX")

  # for each year calculate the PPR score
  for (iy in 1:length(years)) {
    # select the current year
    data <- catch %>% dplyr::filter(YEAR == years[iy])
    # join catch with species(this contains trophic level)
    master <- dplyr::left_join(data,speciesTL,by=speciesCode)

    # calulate index
    PPR$YEAR[iy] <- years[iy]
    # decision rule for missing data. ALL to zero
    master$totLand[is.na(master$totLand)] <- 0
    # index
    PPR$INDEX[iy] <- sum((master$totLand/9)*(1/transferEfficiency)^(master$Troph-1))
  }

  return(PPR)
}
