#' Get landings data (COMLAND)
#'
#' Pulls data from cfdbs, cleans, agregates etc. (comland script) or used a data set already pulled
#' (time saving step)
#'
#'@param channel DBI object. Connection object to database "sole"
#'@param dataPull Boolean. Pull from database(T) or use data from existing pull (F)
#'
#'@section First time use:
#'
#'The very first time this function in used a data pull is required to save the file to your local drive.
#'After which you can reference this file for future work.
#'
#'The initial data pull can take 15 min
#'
#'@section Attention:
#'
#'#' For this to work, you need an oracle client installed.
#' Tested with Oracle instantClient_12_2 installed
#' Note: if you use 64 bit Rstudio then you need a 64 bit client
#' Note: if you use 32 bit Rstudio then you need a 32 bit client
#'
#' You'll also need an account set up on the local server
#'
#'
#' @importFrom magrittr "%>%"
#'
#'@return data frame:
#'
#'\item{YEAR}{}
#'\item{NESPP3}{}
#'\item{EPU}{}
#'\item{SPPLIVMT}{}
#'\item{US}{}
#'
#' @export

get_comland_data <- function(channel=NULL,dataPull=F) {
  if(dataPull) {
    if(is.null(channel)) stop("Please pass an DBI object using the \'channel\' argument ")
    # call comlands function
    # need additional arguments

  }
  # read file from previous pull

  ## EVENTUALLY NEED TO PASS AS ARGUMENTS
  comland <- dplyr::as_tibble(readRDS(here::here("data","comland_meatwt_deflated_EPU.Rds")))
  comland <- comland %>% dplyr::select(YEAR,NESPP3,EPU,SPPLIVMT,US)


  return(comland)

}
