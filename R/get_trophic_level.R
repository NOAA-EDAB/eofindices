#'Obtain trophic level data from rfishbase
#'
#'The trophic level will be obtained for each scientific name provided.
#'Search will be in fishbase for vertebrates and sealifebase for invertebrates.
#'Two fields are accessed from the ecology table, FoodTroph and DietTroph.
#'See fishbase for more details (http://fishbase.us/manual/English/FishbaseThe_ECOLOGY_Table.htm)
#'
#'@param lookupTable dataframe. Any size. One column must be called SCIENTIFIC_NAME. Each row of the table will be cross referenced with fishbase
#'
#'@return Tibble. The same columns as \code{lookupTable} but with following additional columns.
#'
#'\item{genusSpecies}{TRUE if species name is genus species, otherwise FALSE}
#'\item{DietTroph}{Value from DietTroph field in fishbase ecology table}
#'\item{FoodTroph}{Value from FoodTroph field in fishbase ecology table}
#'\item{vertebrate}{TRUE if species found in fishbase, FALSE if found in sealifebase, NA if not found}
#'
#'
#'
#'@export

get_trophic_level <- function(lookupTable) {
  ## Need to deal with Genus, Family level only
  ## Need to output species/Genus with missing Trophic info
  missingSpecies <- vector(mode = "character")

  # create base table, select scientific name for fishbase
  fishbaseTable <- lookupTable |>
    dplyr::mutate(genusSpecies = grepl("\\s+", SCIENTIFIC_NAME)) |>
    dplyr::mutate(
      DietTroph = NA,
      FoodTroph = NA,
      EstTroph = NA,
      vertebrate = NA
    )

  nSpecies <- dim(fishbaseTable)[1]
  #For each scientific name search fishbase
  # look in fishbase and sealifebase and look in ecology table for FoodTroph or DietTroph
  # Fist get fishbase table
  fishbase_data <- rfishbase::load_taxa(server = "fishbase")
  sealife_data <- rfishbase::load_taxa(server = "sealifebase")
  for (isp in 1:nSpecies) {
    print(isp)
    speciesNm <- capitalize_first_letter(fishbaseTable$SCIENTIFIC_NAME[isp])
    print(speciesNm)

    if (fishbaseTable$genusSpecies[isp]) {
      # species name
      # check to see if it is in fishbase
      if (any(fishbase_data$Species == speciesNm)) {
        #vertebrate
        fishbaseTable$vertebrate[isp] <- TRUE
        species_entry <- find_species_trophic_values(
          species_name = speciesNm,
          server_name = "fishbase"
        )
        print(species_entry)
        fishbaseTable$DietTroph[isp] <- species_entry$DietTroph
        fishbaseTable$FoodTroph[isp] <- species_entry$FoodTroph
        fishbaseTable$EstTroph[isp] <- species_entry$EstTroph
      } else if (
        any(
          sealife_data$Species == speciesNm
        )
      ) {
        #invertebrate
        fishbaseTable$vertebrate[isp] <- FALSE
        species_entry <- find_species_trophic_values(
          species_name = speciesNm,
          server_name = "sealifebase"
        )
        fishbaseTable$DietTroph[isp] <- species_entry$DietTroph
        fishbaseTable$FoodTroph[isp] <- species_entry$FoodTroph
        fishbaseTable$EstTroph[isp] <- species_entry$EstTroph
      } else {
        # not in fishbase or sealife base. but is a species Code
        missingSpecies <- rbind(missingSpecies, speciesNm)
        message(paste0(
          "Species name: ",
          speciesNm,
          " doesn't exist in either database."
        ))
      }
    } else {
      # genus or family only
      # find if vertebrate species from this Genus
      speciesNames_verts <- fishbase_data |>
        dplyr::filter(Genus == speciesNm) |>
        dplyr::mutate(sciName = Species) |>
        dplyr::select(sciName) |>
        dplyr::pull()

      speciesNames_verts_family <- fishbase_data |>
        dplyr::filter(Family == speciesNm) |>
        dplyr::mutate(sciName = Species) |>
        dplyr::select(sciName) |>
        dplyr::pull()

      speciesNames_inverts <- sealife_data |>
        dplyr::filter(Genus == speciesNm) |>
        dplyr::mutate(sciName = Species) |>
        dplyr::select(sciName) |>
        dplyr::pull()

      speciesNames_inverts_family <- sealife_data |>
        dplyr::filter(Genus == speciesNm) |>
        dplyr::mutate(sciName = Species) |>
        dplyr::select(sciName) |>
        dplyr::pull()

      if (
        (length(speciesNames_verts) > 0) |
          (length(speciesNames_verts_family) > 0)
      ) {
        # vertebrate
        server_name <- "fishbase"
        if (length(speciesNames_verts) > 0) {
          speciesNames <- speciesNames_verts
        } else {
          speciesNames <- speciesNames_verts_family
        }
        fishbaseTable$vertebrate[isp] <- TRUE
      } else {
        if (length(speciesNames_inverts_family) > 0) {
          speciesNames <- speciesNames_inverts
        } else {
          speciesNames <- speciesNames_inverts_family
        }
        server_name <- "sealifebase"
        fishbaseTable$vertebrate[isp] <- FALSE
      }
      # now select only species in Canada or USA
      speciesNs <- rfishbase::country(
        species_list = as.vector(unlist(speciesNames)),
        server = server_name
      ) |>
        dplyr::filter(country %in% c("Canada", "USA"), Saltwater == 1) |>
        dplyr::select(Species) |>
        dplyr::distinct()

      species_entry <- find_genus_trophic_values(
        species_name = speciesNs,
        server_name = server_name
      )

      fishbaseTable$DietTroph[isp] <- species_entry$DietTroph
      fishbaseTable$FoodTroph[isp] <- species_entry$FoodTroph
      fishbaseTable$EstTroph[isp] <- species_entry$EstTroph
    }
  }

  # create new field called Troph which uses DietToph.
  # If DietTroph == NA then uses FoodTroph.
  # If FoodTroph == NA uses EstTroph.
  print(fishbaseTable)
  fishbaseTable <- fishbaseTable |>
    dplyr::mutate(Troph = select_troph(DietTroph, FoodTroph, EstTroph))

  return(list(fishbaseTable = fishbaseTable, missingSpecies = missingSpecies))
}
