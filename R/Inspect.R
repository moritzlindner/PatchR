#' @describeIn Plots This function plots an interactive \link[=plotly]{plotly} graph from a plot generated using a Build* method.
#' @param what Specifies a plot to be plotted. any ggplot inside the Plots slot can be named
#' @exportMethod Plot_Inspect
setGeneric(name="Plot_Inspect",
           def=function(object,what="I.mon")
           {
             standardGeneric("Plot_Inspect")
           }
)
setMethod("Plot_Inspect",
          "PMRecording",
          definition=function(object,what="I.mon"){
            if (!is.null(object@Plots[[what]])){
              ggplotly(object@Plots[[what]])
            }else{
              warning(paste("Use Build* first to generate ", what))
            }
          }
)

#' @exportMethod Plot_Inspect
setMethod("Plot_Inspect",
          "PMCollection",
          definition=function(object,what="I.mon"){
            if (!is.null(object@Plots[[what]])){
              ggplotly(object@Plots[[what]])
            }else{
              warning(paste("Use Build* first to generate ", what))
            }
          }
)
