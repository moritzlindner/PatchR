#' @describeIn Plot This function plots an interactive \link[=plotly]{plotly} graph from a plot generated using a Build* method.
#' @param what Specifies a plot to be plotted. any ggplot inside the Plots slot can be named
#' @importFrom plotly ggplotly
#' @exportMethod Inspect
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
      plotly::ggplotly(X@Plots[[what]])
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
      plotly::ggplotly(X@Plots[[what]])
    } else{
      warning(paste("Use Plot* first to generate plot for ", what))
    }
  }
)
