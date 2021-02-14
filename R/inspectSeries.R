
setGeneric(name="InspectSeries",
           def=function(object,Trace="I.mon")
           {
             standardGeneric("InspectSeries")
           }
)

#' InspectSeries
#'
#' This function plots an interactive \link[=plotly]{plotly} graph from a plot generated using \link[=BuildSeriesPlot]{BuildSeriesPlot}.
#'
#' @name InspectSeries
#' @param object a \link[=base::matrix]{base::matrix} object
#' @param Trace Specifies a trace to be plotted
#' @import plotly
#' @exportMethod InspectSeries
setMethod("InspectSeries",
          "PMSeries",
          definition=function(object,Trace="I.mon"){
            if (!is.null(object@Plots[[Trace]])){
              ggplotly(object@Plots[[Trace]])
            }else{
              warning(paste("Use BuildSeriesPlot first to buld Series plot for Trace ", Trace))
            }
          }
)
