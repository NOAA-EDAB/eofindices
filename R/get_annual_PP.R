#' Reads primary production data
#'
#'  Kim H provided data aggregated to EPU (ststaistical areas)
#'
#'@param yrs Numeric vector. Years to in which to extract #'@param epu character string. Name of EPU ("GB", "GOM", "MAB", "SS")
#'
#'@return list
#'\item{PP}{data frame. YEAR, ANNUAL_MEAN}
#'\item{EPUarea}{Numeric scalar. Area in Km2}
#'
#'@importFrom magrittr "%>%"
#'
#'@export

get_annual_PP <- function(yrs,epu) {

  PPFileName=list.files(system.file("extdata",package="indexPPR"))

  # read in Kim Hydes Primary production output
  # gC/m2/day
  ppRawData <- read.csv(here::here("data",PPFileName),stringsAsFactors = F)

  # select columns of interest
  ppData <- ppRawData %>%
    dplyr::filter(SUBAREA_NAME == epu,YEAR %in% yrs) %>%
    dplyr::select(YEAR,TOTAL_PIXEL_AREA_KM2,ANNUAL_MEAN) %>%
    dplyr::as_tibble()

  EPUarea <- unique(ppData$TOTAL_PIXEL_AREA_KM2)

  PP <- ppData %>% dplyr::select(YEAR,ANNUAL_MEAN)

  return(list(PP = PP,EPUarea=EPUarea))

}
