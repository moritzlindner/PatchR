#' Interactively view Plots
#'
#' `r lifecycle::badge("stable")` \cr
#' This function plots an interactive \link[=plotly]{plotly} graph from a plot sored in a \linkS4class{PCollection} or \linkS4class{PRecording} object generated using a Build* method.
#' @inheritParams MeasureStimResp
#' @param what Specifies a plot to be plotted. any ggplot inside the Plots slot can be named
#' @seealso \link[=Plot]{Plot}, \link[=Plot]{GetPlot}, \link[=Plot]{GetPlotNames}
#' @importFrom plotly ggplotly
#' @exportMethod Inspect
#' @examples
#' # Build a stimulus response curve from a single recording. Here, a current
#' # response, averaged from 0.8-1s of the trace is used. A PRecording object with
#' # an updated slot Plots is returned.
#' data(PRecording)
#' SampleData<-BuildStimRespPlot(
#'   SampleData,
#'   StimTrace = "V-mon",
#'   RespTrace = "I-mon",
#'   Time = c(0.8,1),
#'   fun = mean,
#'   ReturnPObject = TRUE
#' )
#' # Interactively analyse graph
#' Inspect(SampleData,"StimRespPlot")
#' @name Inspect
setGeneric(
  name = "Inspect",
  def = function(X, what = "I.mon")
  {
    standardGeneric("Inspect")
  }
)

setMethod(
  "Inspect",
  "PRecording",
  definition = function(X, what = "I.mon") {
    if (!is.null(X@Plots[[what]])) {
      ggplotly(X@Plots[[what]])
    } else{
      warning(paste("Use Plot* first to generate plot for ", what))
    }
  }
)

setMethod(
  "Inspect",
  "PCollection",
  definition = function(X, what = "I.mon") {
    if (!is.null(X@Plots[[what]])) {
      ggplotly(X@Plots[[what]])
    } else{
      warning(paste("Use Plot* first to generate plot for ", what))
    }
  }
)
