#' show
#'
#' Default mehtod show for PMSeries
#' @importMethodsFrom methods show
#' @exportMethod show
setMethod("show",
          "PMSeries",
          function(object) {
            cat("An object of class PMSeries \n")
            cat(object@RecordingParams@RecMode, "Experiment \n")
            cat("From", object@RecordingParams@Filename, "\n")
            cat("With", length(getTraces(object)), "Traces ,", length(getSweeps(object)), "Sweeps and", length(getTimeTrace(object)), "Timepoints \n")
            cat("Protocol is ", object@RecordingParams@ProtocolName, "\n")
            cat("Imported on", as.character(as.Date(object@RecordingParams@Created)), "\n")
          }
)

#' @exportMethod show
setMethod("show",
          "PMExperiment",
          function(object) {
            cat("An object of class PMExperiment \n")
            cat(object@RecordingParams@RecMode, "Experiment \n")
            cat("With", length(object@Series), " Series \n")
            cat("in", length(levels(object@Group)), " Groups \n")
            cat("Protocol is ", object@RecordingParams@ProtocolName, "\n")
          }
)
