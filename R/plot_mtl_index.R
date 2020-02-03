#' Plot the mean trophic level index
#'
#'@param MTL data frame. (nx2). columns must be called YEAR, INDEX
#'@param epu character string. Name of EPU ("GB", "GOM", "MAB", "SS")
#'
#'@return NULL
#'
#'@export



plot_mtl_index <- function(MTL,epu){
 

  # plot mean trophic level
  p <- ggplot2::ggplot(data = MTL) + 
    ggplot2::geom_line(mapping = ggplot2::aes(x = YEAR, y = INDEX), size = 1, color = "black") +
    ggplot2::labs(title = paste0("EPU: ",epu,". Threshold = ", threshold)) + 
    ggplot2::ylab("Mean Trophic Level")
  
 return(p)
  
}