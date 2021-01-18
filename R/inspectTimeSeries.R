
setGeneric(name="inspectTimeSeries",
           def=function(object,Channel="I.mon")
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
#' @param Channel channel to plot
#' @import plotly
#' @exportMethod inspectTimeSeries
setMethod("inspectTimeSeries",
          "PMTrace",
          definition=function(object,Channel="I.mon"){
            if (!is.null(object@Plots[[Channel]])){
              plotly(object@Plots[[Channel]])
            }else{
              warning(paste("Use BuildTimeSeriesPlot first to buld Time series plot for channel ", Channel))
            }
          }
)
