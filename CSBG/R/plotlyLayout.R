#' plotlyLayout  Maintainence Funstion adding caption to ployly plots
#' @param CrtTit Chart token name
#' @param xAx X-axis token name
#' @param yAx Y axis token name
#' @param Cap Caption token name
#' @paaram Legend Legend Value (TRUE or FALSE)
#' 
#' @return layout statement for plotly graph
#' @export

plotlyLayout <- function(CrtTit,xAx,yAx,Cap,Legend) {
  
  outStr <- paste0("layout(title = ", CrtTit,
                   ", xaxis = ",xAx,
                   ", yaxis = ",yAx,
                   ", showlegend = ",Legend," hoverlabel = 'right', margin = list(l = 50, r = 50, t = 60, b = 110),  
                      annotations = list(text =",Cap,
                   ",  font = list(size = 12), showarrow = FALSE, xref = 'paper', x = 0, yref = 'paper', y = -0.3)" )
 return(outStr)
}
