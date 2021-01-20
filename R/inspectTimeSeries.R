
setGeneric(name="InspectTimeSeries",
           def=function(object,Trace="I.mon")
           {
             standardGeneric("InspectTimeSeries")
           }
)

#' InspectTimeSeries
#'
#' This function creates an interactive graph from a plot generated using BuildTimeSeriesPlot
#'
#' @name InspectTimeSeries
#' @param object a PMSeries object
#' @param Trace Trace to plot
#' @import plotly
#' @exportMethod InspectTimeSeries
setMethod("InspectTimeSeries",
          "PMSeries",
          definition=function(object,Trace="I.mon"){
            if (!is.null(object@Plots[[Trace]])){
              ggplotly(object@Plots[[Trace]])
            }else{
              warning(paste("Use BuildTimeSeriesPlot first to buld Time series plot for Trace ", Trace))
            }
          }
)
