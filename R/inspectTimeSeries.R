
setGeneric(name="inspectTimeSeries",
           def=function(object,Trace="I.mon")
           {
             standardGeneric("inspectTimeSeries")
           }
)

#' inspectTimeSeries
#'
#' This function creates an interactive graph from a plot generated using BuildTimeSeriesPlot
#'
#' @name inspectTimeSeries
#' @param object a PMSeries object
#' @param Trace Trace to plot
#' @import plotly
#' @exportMethod inspectTimeSeries
setMethod("inspectTimeSeries",
          "PMSeries",
          definition=function(object,Trace="I.mon"){
            if (!is.null(object@Plots[[Trace]])){
              ggplotly(object@Plots[[Trace]])
            }else{
              warning(paste("Use BuildTimeSeriesPlot first to buld Time series plot for Trace ", Trace))
            }
          }
)
