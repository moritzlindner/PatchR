#' Show
#'
#' Default method \code{show} for \link[=PRecording]{PRecording} and \link[=PCollection]{PCollection} objects.
#'
#' @importMethodsFrom methods show
#' @param object An S4 object of type \link[=PRecording]{PRecording} or \link[=PCollection]{PCollection}
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
              "Traces/Channels ,",
              length(GetSweepNames(object)),
              "Sweeps and",
              length(GetTimeTrace(object)),
              "Timepoints \n"
            )
            cat(
              "RSeal: ",
              ConvenientScalesvalue(object@RecordingParams@RSeal),
              ConvenientScalessi(object@RecordingParams@RSeal),
              ", Rs: ",
              ConvenientScalesvalue(object@RecordingParams@Rs),
              ConvenientScalessi(object@RecordingParams@Rs),
              ", Cs: ",
              ConvenientScalesvalue(object@RecordingParams@Cs),
              ConvenientScalessi(object@RecordingParams@Cs),
              ", URest: ",
              ConvenientScalesvalue(object@RecordingParams@URest),
              ConvenientScalessi(object@RecordingParams@URest),
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
            cat("With", length(object@Recordings), " Recordings \n")
            cat("in", length(levels(object@Group)), " Groups \n")
            cat("Protocol is ", object@RecordingParams@ProtocolName, "\n")
          })
