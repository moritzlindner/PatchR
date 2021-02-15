
setGeneric(name="Inspect",
           def=function(object,what="I.mon")
           {
             standardGeneric("Inspect")
           }
)

#' Inspect
#'
#' This function plots an interactive \link[=plotly]{plotly} graph from a plot generated using a Build method.
#'
#' @param object a \link[=base::matrix]{base::matrix} object
#' @param what Specifies a plot to be plotted. any ggplot inside the Plots slot can be named
#' @import plotly
#' @exportMethod Inspect
setMethod("Inspect",
          "PMRecording",
          definition=function(object,what="I.mon"){
            if (!is.null(object@Plots[[what]])){
              ggplotly(object@Plots[[what]])
            }else{
              warning(paste("Use Build* first to generate ", what))
            }
          }
)

#' @exportMethod Inspect
setMethod("Inspect",
          "PMCollection",
          definition=function(object,what="I.mon"){
            if (!is.null(object@Plots[[what]])){
              ggplotly(object@Plots[[what]])
            }else{
              warning(paste("Use Build* first to generate ", what))
            }
          }
)
