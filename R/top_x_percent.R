#' Select species in top x of landings
#'
#'A list of the most abundance species (by landings).
#'The number of species selected is determined by the fraction of cumulative sum
#'of the landings. This fraction is controlled by the value of threshold
#'
#'@param NESPP3 character vector. NESPP3 codes
#'@param LANDINGS numeric vector. landings associated with NESPP3 codes
#'@param threshold numeric scalar. fraction of landings used to determin the most abundant species in landings
#'
#'@return Tibble (nx2):
#'
#'\item{NESPP3}{species code}
#'\item{LANDINGS}{landings associates with NESPP3 code}
#'
#' @importFrom magrittr "%>%"
#'
#'@export


top_x_percent <- function(NESPP3,LANDINGS,threshold){

  d <- data.frame(NESPP3 = NESPP3,LANDINGS = LANDINGS,stringsAsFactors = F)

  newd <- d %>%
    dplyr::arrange(desc(LANDINGS)) %>%
    dplyr::mutate(cum_sum = cumsum(LANDINGS)) %>%
    dplyr::mutate(percent=cum_sum/sum(LANDINGS)) %>%
    dplyr::mutate(flag= percent<= threshold)

  ind <- sum(newd$flag==TRUE) + 1
  newd$flag[ind] <- TRUE

  species <- newd %>% dplyr::filter(flag == TRUE) %>%
    dplyr::select(NESPP3,LANDINGS)

  return(dplyr::as_tibble(species))

}
