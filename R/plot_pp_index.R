#' Plot the Primary Production 
#' 
#'@param scaled data frame. (nx2). columns must be called YEAR, ANNUAL_MEAN
#'@param epu character string. Name of EPU ("GB", "GOM", "MAB", "SS")
#'
#'@return NULL
#'
#'@export

plot_pp_index <- function(scaled,epu){
  
  ## primary production
  
  p <- ggplot2::ggplot(data = scaled) +
    ggplot2::geom_line(mapping = ggplot2::aes(x=YEAR, y=ANNUAL_MEAN)) +
    ggplot2::labs(title=epu) +
    ggplot2::ylab(expression(paste("Primary Production (gC ",m^-2,d^-1,")" ) )) +
    ggplot2::xlab("")
  
  #plot(p)
  return(p)
  
}