#' Show
#'
#' Default method \code{show} for \link[=PRecording]{PRecording} and \link[=PCollection]{PCollection} objects.
#'
#' @importMethodsFrom methods show
#' @inheritParams Get
#' @exportMethod show
#' @name show
NULL

#' @noMd
setMethod("show",
          "PRecording",
          function(X) {
            cat("An object of class PRecording \n")
            cat(X@RecordingParams@RecMode, "Experiment \n")
            cat("From", X@RecordingParams@Filename, "\n")
            cat(
              "With",
              length(GetTraceNames(X)),
              "Traces/Channels ,",
              length(GetSweepNames(X)),
              "Sweeps and",
              length(GetTimeTrace(X)),
              "Timepoints \n"
            )
            cat(
              "RSeal: ",
              ConvenientScalesvalue(X@RecordingParams@RSeal),
              ConvenientScalessi(X@RecordingParams@RSeal),
              ", Rs: ",
              ConvenientScalesvalue(X@RecordingParams@Rs),
              ConvenientScalessi(X@RecordingParams@Rs),
              ", Cs: ",
              ConvenientScalesvalue(X@RecordingParams@Cs),
              ConvenientScalessi(X@RecordingParams@Cs),
              ", URest: ",
              ConvenientScalesvalue(X@RecordingParams@URest),
              ConvenientScalessi(X@RecordingParams@URest),
              "\n"
            )
            cat("Protocol is ", X@RecordingParams@ProtocolName, "\n")
            cat("Imported on", as.character(as.Date(X@RecordingParams@Created)), "\n")
          })
#' @noMd
#' @exportMethod show
setMethod("show",
          "PCollection",
          function(X) {
            cat("An Object of class PCollection \n")
            cat(X@RecordingParams@RecMode, "Experiment \n")
            cat("With", length(X@Recordings), " Recordings \n")
            cat("in", length(levels(X@Group)), " Groups \n")
            cat("Protocol is ", X@RecordingParams@ProtocolName, "\n")
          })
