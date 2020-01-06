#'Combine trophic level data
#'
#'Fishbase contains Trophic level data in several locations.
#'We use the field: DietTroph as our main measure of Trophic Level.
#'If this is absent we resort to the field: FoodTroph
#'
#'@param DietTroph Numeric vector. Value of Trophic level
#'@param FoodTroph Numeric vector. Value of Trophic level
#'
#'@return Numeric vector. combined vectors of Trophic level
#'
#'@export

select_troph <- function(DietTroph,FoodTroph){
  troph <- DietTroph
  ind <- is.na(DietTroph)
  troph[ind] <- FoodTroph[ind]
  return(troph)
}
