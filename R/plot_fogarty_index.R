#' Plot the Fogarty index
#'
#'@param data Data frame. columns must be called YEAR, INDEX, landings
#'@param epu character string. Name of EPU ("GB", "GOM", "MAB", "SS")
#'@param yearField Character string. The name of the field in \code{data} which contains the Year.
#'@param indexField Character string. The name of the field in \code{data} which contains the value of the index
#'
#'
#'@return ggplot Object
#'
#'@export

plot_fogarty_index <- function(data, epu, yearField="YEAR",indexField="Index"){


  p <- ggplot2::ggplot(data = data) +
    ggplot2::geom_line(mapping = ggplot2::aes(x=get(yearField), y=get(indexField)*1000)) +
    ggplot2::labs(title=paste0("Fogarty Index: ",epu)) +
    ggplot2::ylab("Parts per thousand (0/00) ") +
    ggplot2::geom_hline(yintercept = 0.22,linetype="dashed",color="green")+
    ggplot2::geom_hline(yintercept = 0.92,linetype="dashed",color = "green")+
    ggplot2::geom_hline(yintercept = 1.0,linetype="dashed",color = "red")+
    ggplot2::geom_hline(yintercept = 2.5,linetype="dashed",color="red")+
    ggplot2::xlab("")

  return(p)


}
