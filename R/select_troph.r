#'Combine trophic level data
#'
#'Fishbase contains Trophic level data in several locations.
#'We use the field: DietTroph as our main measure of Trophic Level (ecology table)
#'If this is absent we resort to the field: FoodTroph (ecology table)
#'then finally EstTroph (estimate table)
#'
#'@param DietTroph Numeric vector. Value of Trophic level
#'@param FoodTroph Numeric vector. Value of Trophic level
#'@param EstTroph Numeric vector. Value of Trophic level
#'
#'@return Numeric vector. combined vectors of Trophic level
#'@export


select_troph <- function(DietTroph,FoodTroph,EstTroph){
  troph <- DietTroph
  # if Dietroph is missing use FoodTroph
  ind <- is.na(DietTroph)
  troph[ind] <- FoodTroph[ind]
  # if still missing then use EstTroph
  ind <- is.na(DietTroph)
  troph[ind] <- EstTroph[ind]
  return(troph)
}
