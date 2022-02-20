#' (OK) Show
#'
#' Default method show for PRecording
#'
#' @importMethodsFrom methods show
#' @param object An S4 object of type PRecording or PCollection
#' @exportMethod show
#' @name show
NULL

#' @noMd
setMethod("show",
          "PRecording",
          function(object) {
            cat("An object of class PRecording \n")
            cat(object@RecordingParams@RecMode, "Experiment \n")
            cat("From", object@RecordingParams@Filename, "\n")
            cat(
              "With",
              length(GetTraceNames(object)),
              "Traces ,",
              length(GetSweepNames(object)),
              "Sweeps and",
              length(GetTimeTrace(object)),
              "Timepoints \n"
            )
            cat(
              "RSeal: ",
              ConvenientScalesvalue(rec@RecordingParams@RSeal),
              ConvenientScalessi(rec@RecordingParams@RSeal),
              ", Rs: ",
              ConvenientScalesvalue(rec@RecordingParams@Rs),
              ConvenientScalessi(rec@RecordingParams@Rs),
              ", Cs: ",
              ConvenientScalesvalue(rec@RecordingParams@Cs),
              ConvenientScalessi(rec@RecordingParams@Cs),
              ", URest: ",
              ConvenientScalesvalue(rec@RecordingParams@URest),
              ConvenientScalessi(rec@RecordingParams@URest),
              "\n"
            )
            cat("Protocol is ", object@RecordingParams@ProtocolName, "\n")
            cat("Imported on", as.character(as.Date(object@RecordingParams@Created)), "\n")
          })
#' @noMd
#' @exportMethod show
setMethod("show",
          "PCollection",
          function(object) {
            cat("An object of class PCollection \n")
            cat(object@RecordingParams@RecMode, "Experiment \n")
            cat("With", length(object@Series), " Series \n")
            cat("in", length(levels(object@Group)), " Groups \n")
            cat("Protocol is ", object@RecordingParams@ProtocolName, "\n")
          })
