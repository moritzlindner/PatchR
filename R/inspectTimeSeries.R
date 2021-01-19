
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
#' @param object a PMTrace object
#' @param Trace Trace to plot
#' @import plotly
#' @exportMethod inspectTimeSeries
setMethod("inspectTimeSeries",
          "PMTrace",
          definition=function(object,Trace="I.mon"){
            if (!is.null(object@Plots[[Trace]])){
              plotly(object@Plots[[Trace]])
            }else{
              warning(paste("Use BuildTimeSeriesPlot first to buld Time series plot for Trace ", Trace))
            }
          }
)
