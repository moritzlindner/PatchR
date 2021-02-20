#' show
#'
#' Default mehtod show for PRecording
#'
#' @importMethodsFrom methods show
#' @title show
#' @exportMethod show
NULL
setMethod("show",
          "PRecording",
          function(object) {
            cat("An object of class PRecording \n")
            cat(object@RecordingParams@RecMode, "Experiment \n")
            cat("From", object@RecordingParams@Filename, "\n")
            cat("With", length(GetTraceNames(object)), "Traces ,", length(GetSweepNames(object)), "Sweeps and", length(GetTimeTrace(object)), "Timepoints \n")
            cat("Protocol is ", object@RecordingParams@ProtocolName, "\n")
            cat("Imported on", as.character(as.Date(object@RecordingParams@Created)), "\n")
          }
)

#' @exportMethod show
setMethod("show",
          "PCollection",
          function(object) {
            cat("An object of class PCollection \n")
            cat(object@RecordingParams@RecMode, "Experiment \n")
            cat("With", length(object@Series), " Series \n")
            cat("in", length(levels(object@Group)), " Groups \n")
            cat("Protocol is ", object@RecordingParams@ProtocolName, "\n")
          }
)
