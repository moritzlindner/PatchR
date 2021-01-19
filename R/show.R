#' show
#'
#' Default mehtod show for PMSeries
#' @importMethodsFrom methods show
#' @exportMethod show
setMethod("show",
          "PMSeries",
          function(object) {
            cat("An object of class PMSeries")
            cat(object@RecordingParams@RecMode, "Experiment \n")
            cat("From", object@RecordingParams@Filename, "\n")
            cat("With", length(getTraces(object)), "Traces ,", length(getSweeps(object)), "Sweeps and", length(getTimeTrace(object)), "Timepoints \n")
            cat("Protocol is ", object@RecordingParams@ProtocolName, "\n")
            cat("Imported on", as.character(as.Date(object@RecordingParams@Created)), "\n")
          }
)
