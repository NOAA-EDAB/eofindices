#' Plot the Ryther index
#'
#'@param data Data frame. columns must be called YEAR, INDEX, landings
#'@param epu Character string. Name of EPU ("GB", "GOM", "MAB", "SS")
#'@param yearField Character string. The name of the field in \code{data} which contains the Year.
#'@param indexField Character string. The name of the field in \code{data} which contains the value of the index
#'
#'
#'@return ggplot Object
#'
#'@export

plot_ryther_index <- function(data, epu, yearField="YEAR",indexField="Index"){


  p <- ggplot2::ggplot(data = data) +
    ggplot2::geom_line(mapping = ggplot2::aes(x=get(yearField), y=get(indexField))) +
    ggplot2::geom_hline(yintercept = 0.3,linetype="dashed",color="green")+
    ggplot2::geom_hline(yintercept = 1.1,linetype="dashed",color = "green")+
    ggplot2::geom_hline(yintercept = 3,linetype="dashed",color="red")+
    ggplot2::geom_hline(yintercept = 5,linetype="dashed",color="red")+
    ggplot2::labs(title=paste0("Ryther Index: ",epu)) +
    ggplot2::ylab(expression(paste("Landings (mt ",km^-2,y^-1,")" ) )) +
    ggplot2::xlab("")

  return(p)


}
