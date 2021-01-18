#' show
#'
#' Default mehtod show for PMTrace
#' @exportMethod show
setMethod("show",
          "PMTrace",
          function(object) {
            cat("An object of class PMTrace")
            cat(object@RecordingParams@RecMode, "Experiment \n")
            cat("From", object@RecordingParams@Filename, "\n")
            cat("With", length(getChannels(object)), "Channels ,", length(getSweeps(object)), "Sweeps and", length(getTimeTrace(object)), "Timepoints \n")
            cat("Protocol is ", object@RecordingParams@ProtocolName, "\n")
            cat("Imported on", as.character(as.Date(object@RecordingParams@Created)), "\n")
          }
)
