#' show
#'
#' Default mehtod show for PMRecording
#' @importMethodsFrom methods show
#' @exportMethod show
setMethod("show",
          "PMRecording",
          function(object) {
            cat("An object of class PMRecording \n")
            cat(object@RecordingParams@RecMode, "Experiment \n")
            cat("From", object@RecordingParams@Filename, "\n")
            cat("With", length(getTraceNames(object)), "Traces ,", length(getSweepNames(object)), "Sweeps and", length(getTimeTrace(object)), "Timepoints \n")
            cat("Protocol is ", object@RecordingParams@ProtocolName, "\n")
            cat("Imported on", as.character(as.Date(object@RecordingParams@Created)), "\n")
          }
)

#' @exportMethod show
setMethod("show",
          "PMCollection",
          function(object) {
            cat("An object of class PMCollection \n")
            cat(object@RecordingParams@RecMode, "Experiment \n")
            cat("With", length(object@Series), " Series \n")
            cat("in", length(levels(object@Group)), " Groups \n")
            cat("Protocol is ", object@RecordingParams@ProtocolName, "\n")
          }
)
