#' Plot the Primary Production Required index (scaled)
#'
#'@param scaled data frame. (nx3). columns must be called YEAR, INDEX, landings
#'@param epu character string. Name of EPU ("GB", "GOM", "MAB", "SS")
#'@param withLandings Boolean. Create a plot with second axis, including landings ("T"). Defaul = F
#'
#'@return NULL
#'
#'@export

plot_ppr_index <- function(scaled, epu, withLandings=F){

  if(!withLandings) {
    # plot scaled PPR index
    p <- ggplot2::ggplot(data = scaled) + 
      ggplot2::geom_line(mapping = ggplot2::aes(x = YEAR, y = SCALEDINDEX), size = 1, color = "black") +
      ggplot2::ylab("Proportion of Total Primary Production") +
      ggplot2::labs(title = paste0("EPU: ",epu,". Threshold = ", threshold))
  } else {
    ## plot landings and scaled PPR 
    scalingFactorPPR <- max(scaled$landings/10^6)/max(scaled$SCALEDINDEX,na.rm = T)
    p <- ggplot2::ggplot(data = scaled) + 
      ggplot2::geom_line(mapping = ggplot2::aes(x = YEAR, y = landings/10^6), size = 1, color = "grey") +
      ggplot2::geom_line(mapping = ggplot2::aes(x = YEAR, y = SCALEDINDEX*scalingFactorPPR), size = 1, color = "black") +
      ggplot2::scale_y_continuous(name = "Landings (Metric Tons)", 
                                sec.axis = ggplot2::sec_axis(~(./scalingFactorPPR), name = expression(paste("Proportion of Total Primary Production")))) +
      ggplot2::theme(
      axis.title.y = ggplot2::element_text(color = "grey"),
      axis.title.y.right = ggplot2::element_text(color = "black"))+
      ggplot2::labs(title = paste0("EPU: ",epu,". Threshold = ", threshold))
  }
  
  return(p)

  
}