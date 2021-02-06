
setGeneric(name="InspectTimeSeries",
           def=function(object,Trace="I.mon")
           {
             standardGeneric("InspectTimeSeries")
           }
)

#' InspectTimeSeries
#'
#' This function plots an interactive \link[=plotly]{plotly} graph from a plot generated using \link[=BuildTimeSeriesPlot.]{BuildTimeSeriesPlot}
#'
#' @name InspectTimeSeries
#' @param object a \link[=base::matrix]{base::matrix} object
#' @param Trace Specifies a trace to be plotted
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
