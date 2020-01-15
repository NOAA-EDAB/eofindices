#' Explores species composition based on landings
#'
#' Species that comprise the top x% of landings for each year are extracted.
#'
#'
#'@param channel DBI object. Connection object to database "sole"
#'@param landings dataframe (n x r). r >= 3. Must have at least three columns (one named YEAR) The other two necessary columns numst contain Species codes and Catch (units)
#'@param threshold numeric scalar. Determining the fraction of landings to be represented. Eg. threshold = 0.8
#'would find the fewest number of species (ordered by landings) that comprise 80\% of landings.
#'@param filename character string. Filename of exported figure
#'@param speciesCodeCN Character string. The name of the column in datafraem that contains the Species codes. Default = "NESPP3"
#'@param catchCN Character string. The name of the column that contains the catch. Default = "CATCH"
#'@param ... Other arguments passed to ggplot::ggsave
#'
#'
#'@section Attention:
#'
#' For this to work, you need an oracle client installed.
#' Tested with Oracle instantClient_12_2 installed
#' Note: if you use 64 bit Rstudio then you need a 64 bit client
#' Note: if you use 32 bit Rstudio then you need a 32 bit client
#'
#' You'll also need an account set up on the local server
#'
#'
#'@return List:
#'\item{p}{ggplot object}
#'\item{data}{Tibble (n x 5). Columns: YEAR,NESPP3,LANDINGS,COMNAME,PresenceAbsence}
#'
#'@export

explore_species_composition <- function(channel,landings,threshold,filename=NULL,speciesCodeCN="NESPP3",catchCN="CATCH", ...) {

  # rename the column containing speciescode
  names(landings)[names(landings) == speciesCodeCN] <- "NESPP3"
  names(landings)[names(landings) == catchCN] <- "CATCH"

  years <- seq(from=min(landings$YEAR),to=max(landings$YEAR), by=1)
  topx <- data.frame(YEAR=NULL,NESPP3=NULL,LANDINGS=NULL)
  for (iy in years) {
    dataTop <- landings %>% dplyr::filter(YEAR == iy)
    species <- select_top_x_percent(dataTop$NESPP3,dataTop$CATCH,threshold)
    newYear <- data.frame(YEAR=as.integer(rep(iy,length(species$NESPP3))),NESPP3=species$NESPP3,LANDINGS=species$LANDINGS,stringsAsFactors=F)
    topx <- rbind(topx,newYear)
  }
  # plot topx species comp over time and landings
  # expand grid

  topxnespp3s <- topx %>% dplyr::distinct(NESPP3)

  lookupSpecies <- dbutils::create_species_lookup(channel,species=unlist(topxnespp3s))
  commonNames <- lookupSpecies$data %>% dplyr::select(NESPP3,COMMON_NAME) %>% dplyr::distinct()

  timeSpeciesGrid <- expand.grid(YEAR=min(years):max(years),NESPP3=topxnespp3s$NESPP3,stringsAsFactors = F)
  topxGrid <- dplyr::as_tibble(dplyr::left_join(timeSpeciesGrid,topx,by=c("YEAR","NESPP3")))
  topxGrid <- dplyr::left_join(topxGrid,commonNames,by="NESPP3")
  # create a presence-absence column
  topxGrid <- topxGrid %>% dplyr::mutate(PresenceAbsence=!is.na(LANDINGS))
  yearAgg <- topxGrid %>% dplyr::group_by(YEAR) %>% dplyr::summarize(totLand=sum(LANDINGS,na.rm = T))

  p <- ggplot2::ggplot(data=topxGrid) +
    ggplot2::geom_point(mapping=ggplot2::aes(x=YEAR,y=COMMON_NAME,color=PresenceAbsence)) +
    ggplot2::scale_color_manual(values=c("white", "darkgreen")) +
    ggplot2::geom_line(data=yearAgg,ggplot2::aes(x=YEAR,y=1+length(unlist(topxnespp3s))+totLand/100000)) +
    ggplot2::theme(legend.position = "none",axis.title.x=ggplot2::element_blank(),axis.title.y=ggplot2::element_blank())

  if (!is.null(filename)) {
    ggplot2::ggsave(filename=filename,plot=p, ...)
  }

  names(topxGrid)[names(topxGrid) == "NESPP3"] <- speciesCodeCN
  names(topxGrid)[names(topxGrid) == "CATCH"] <- catchCN

  return(list(plotObj = p,data=topxGrid))



}
