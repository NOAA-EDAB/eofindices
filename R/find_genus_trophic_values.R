#' Find all Trophic value data in fishbase for a species
#'
#' Trophic level data is found in different locations, the ecology table and the estimate table
#' All data is retrieved for a given species
#'
#' @param species_name Character string. Latin name of species
#' @param server_name Character. The name of the server to access "fishbase" for vertibrates and "sealifebase" for invertibrates
#'
#' @noRd

find_genus_trophic_values <- function(species_name, server_name) {
  species_entry <- NULL

  # search ECOLOGY fishbase and sealifebase
  ecology_values <- rfishbase::ecology(
    species_list = as.vector(unlist(species_name)),
    server = "fishbase"
  ) |>
    dplyr::select(DietTroph, FoodTroph)

  # search ESTIMATE table as a back up
  est_values <- rfishbase::estimate(
    species_list = as.vector(unlist(species_name)),
    server = "fishbase"
  ) |>
    dplyr::select(Troph)

  # take the mean since there could be multiple entries if checking at genus or family level
  species_entry$DietTroph <- mean(
    ecology_values$DietTroph,
    na.rm = TRUE
  )
  species_entry$FoodTroph <- mean(
    ecology_values$FoodTroph,
    na.rm = TRUE
  )
  species_entry$EstTroph <- mean(est_values$Troph, na.rm = TRUE)

  # replace potential NaN values with NAs
  species_entry[is.na(species_entry)] <- NA

  return(species_entry)
}
