#'Obtain trophic level data from rfishbase
#'
#'The trophic level will be obtained for each scientific name provided.
#'Search will be in fishbase for vertibrates and sealifebase for invertibrates.
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
#'\item{vertibrate}{TRUE if species found in fishbase, FALSE if found in sealifebase, NA if not found}
#'
#'
#' @importFrom magrittr "%>%"
#'
#'@export

get_trophic_level <- function(lookupTable) {

  ## Need to deal with Genus, Family level only
  ## Need to output species/Genus with missing Trophic info
  missingSpecies <- vector(mode = "character")

  # create base table, select scientific name for fishbase
  fishbaseTable <- lookupTable %>%
    dplyr::mutate(genusSpecies = grepl("\\s+",SCIENTIFIC_NAME)) %>%
    dplyr::mutate(DietTroph=NA,FoodTroph=NA,EstTroph=NA,vertibrate=NA)

  nSpecies <- dim(fishbaseTable)[1]
  #For each scientific name search fishbase
  # look in fishbase and sealifebase and look in ecology table for FoodTroph or DietTroph
  for (isp in 1:nSpecies) {
    speciesNm <- capitalize_first_letter(fishbaseTable$SCIENTIFIC_NAME[isp])
    print(speciesNm)
    if (fishbaseTable$genusSpecies[isp]) { # species name
      # check to see if it is in fishbase
      if(any(paste(rfishbase::fishbase$Genus,rfishbase::fishbase$Species)==speciesNm)) {
        #vertibrate
        fishbaseTable$vertibrate[isp] <- T
        # search fishbase and sealifebase
        vertibrates <- rfishbase::ecology(species_list=speciesNm, server = "fishbase") %>%
          dplyr::select(DietTroph,FoodTroph)
        # estimate table.
        est <- rfishbase::estimate(species_list=speciesNm, server = "fishbase") %>%
          dplyr::select(Troph)
        vertibrates <- cbind(vertibrates,est)
        if(any(!is.na(vertibrates))) {
          if((dim(vertibrates)[1])>1) {vertibrates <- data.frame(as.list(colMeans(vertibrates,na.rm = T)))}
          fishbaseTable$DietTroph[isp] <- vertibrates$DietTroph
          fishbaseTable$FoodTroph[isp] <- vertibrates$FoodTroph
          fishbaseTable$EstTroph[isp] <- est$Troph
        }

      } else if (any(paste(rfishbase::sealifebase$Genus,rfishbase::sealifebase$Species)==speciesNm)) { #invertibrate
        fishbaseTable$vertibrate[isp] <- F
        invertibrates <- rfishbase::ecology(species_list=speciesNm, server = "sealifebase") %>%
          dplyr::select(DietTroph,FoodTroph)
        est <- rfishbase::estimate(species_list=speciesNm, server = "sealifebase") %>%
          dplyr::select(Troph)
        invertibrates <- cbind(invertibrates,est)
        if(any(!is.na(invertibrates))){
          if((dim(invertibrates)[1])>1) {invertibrates <- data.frame(as.list(colMeans(vertibrates,na.rm = T)))}
          fishbaseTable$DietTroph[isp] <- invertibrates$DietTroph
          fishbaseTable$FoodTroph[isp] <- invertibrates$FoodTroph
          fishbaseTable$EstTroph[isp] <- est$Troph

        }
      } else {
        # not in fishbase or sealife base. but is a species Code
        missingSpecies <- rbind(missingSpecies,speciesNm)
        message(paste0("Species name: ",speciesNm, " doesn't exist in either database."))
      }

    } else {
      # genus only
      # find all species from this Genus
      speciesNames <- rfishbase::fishbase %>%
        dplyr::filter(Genus == speciesNm) %>%
        dplyr::mutate(sciName = paste(Genus,Species)) %>%
        dplyr::select(sciName)
      # now select only species in Canda or USA
      speciesNs <- rfishbase::country(species_list=as.vector(unlist(speciesNames)),server="fishbase") %>%
        dplyr::filter(country %in% c("Canada","USA"), Saltwater == 1) %>%
        dplyr::select(Species) %>%
        dplyr::distinct()

      # search ECOLOGY table and take mean of duplicates (Error in Fishbase???)
      vertibrates <- rfishbase::ecology(species_list=as.vector(unlist(speciesNs)), server = "fishbase") %>%
        dplyr::select(Species,DietTroph,FoodTroph) %>%
        dplyr::group_by(Species) %>%
        dplyr::summarise(DietTroph=mean(DietTroph),FoodTroph=mean(FoodTroph)) %>%
        dplyr::select(DietTroph,FoodTroph) %>%
        dplyr::ungroup()

      # search ESTIMATE table as a back up
      est <- rfishbase::estimate(species_list=as.vector(unlist(speciesNs)), server = "fishbase") %>%
        dplyr::select(Troph)

      vertibrates <- cbind(vertibrates,est)

      if (any(!is.na(vertibrates))) {
        vertibrates <- data.frame(as.list(colMeans(vertibrates,na.rm = T)))
        fishbaseTable$DietTroph[isp] <- vertibrates$DietTroph
        fishbaseTable$FoodTroph[isp] <- vertibrates$FoodTroph
        fishbaseTable$EstTroph[isp] <- vertibrates$FoodTroph
        fishbaseTable$vertibrate[isp] <- T
      } else {
        # inverts

        speciesNames <- rfishbase::sealifebase %>%
          dplyr::filter(Genus == speciesNm) %>%
          dplyr::mutate(sciName = paste(Genus,Species)) %>%
          dplyr::select(sciName)
        # now select only species in Canda or USA
        speciesNs <- rfishbase::country(species_list=as.vector(unlist(speciesNames)),server="sealifebase") %>%
          dplyr::filter(country %in% c("Canada","USA"), Saltwater == 1) %>%
          dplyr::select(Species) %>%
          dplyr::distinct()


        invertibrates <- rfishbase::ecology(species_list=as.vector(unlist(speciesNs)), server = "sealifebase") %>%
          dplyr::select(DietTroph,FoodTroph)
        est <- rfishbase::estimate(species_list=as.vector(unlist(speciesNs)), server = "sealifebase") %>%
          dplyr::select(Troph)
        invertibrates <- cbind(invertibrates,est)

        if (any(!is.na(invertibrates))) {
          invertibrates <- data.frame(as.list(colMeans(invertibrates,na.rm = T)))
          fishbaseTable$DietTroph[isp] <- invertibrates$DietTroph
          fishbaseTable$FoodTroph[isp] <- invertibrates$FoodTroph
          fishbaseTable$vertibrate[isp] <- F
        }
      }
    }
  }

  # create new field called Troph which uses DietToph.
  # If DietRoph == NA then uses FoodTroph.
  # If FoodTroph == NA uses estTroph.
  fishbaseTable <- fishbaseTable %>%
    dplyr::mutate(Troph = select_troph(DietTroph,FoodTroph,EstTroph=NULL))


  return(list(fishbaseTable=fishbaseTable,missingSpecies=missingSpecies))

}
