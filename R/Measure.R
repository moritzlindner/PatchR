#' Measurement methods
#'
#' `r lifecycle::badge("experimental")` \cr
#' These methods perform measurements on \linkS4class{PRecording} and \linkS4class{PCollection} objects on a per-sweep basis. They are designed to efficiently collect data e.g. for time series, dose-response or point statistics
#'
#' @inheritParams GetData
#' @param Trace The name of the \code{trace} to perform measurements on
#' @param StimTrace The name of the \code{trace} that contains the stimulus
#' @param RespTrace The name of the \code{trace} that contains the response
#' @param label A label (if \code{Sweeps} has length 1) or a prefix
#' @param FUN function to apply on sweep. Can be anything that woks with \link[base:apply]{apply()}. But will be usually be \link[base:mean]{mean()}, \link[base:max]{max()}, \link[base:min]{min()}, or \link[base:Arithmetic]{-} .
#' @param ReturnPObject whether to return a \linkS4class{PRecording}/\linkS4class{PCollection} or a matrix.
#' @examples
#' data("PRecording")
#' # Get values needed for a Stimulus-Response-Curve. Apply function (FUN) mean to
#' # the interval 0.8s -1s (end of stimulus) from each sweep of V-mon (StimTrace)
#' # and I-mon (RespTrace)
#' MeasureStimResp(SampleData,
#'                 StimTrace = "V-mon",
#'                 RespTrace = "I-mon",
#'                 Time = c(0.8,1),
#'                 FUN = mean)
#' @name Measure
NULL

#' @describeIn Measure This function performs the actual measurements. It subsets an object by \code{Trace} and \code{Sweeps}. If \code{FUN} is a binary operator, then applies it to the exact time points given in \code{Time}, else, it applies it to the range indicated by \code{Time}.
#' @return MeasureSweeps: A \linkS4class{PRecording} or \linkS4class{PCollection}, respectiveley with an updated MetaData Slot or, if \code{ReturnPObject=F} a matrix.
#' @exportMethod MeasureSweeps
setGeneric(
  name = "MeasureSweeps",
  def = function(X,
                 Trace,
                 Sweeps = GetSweepNames(X),
                 Time,
                 label,
                 FUN = mean,
                 ReturnPObject = T)
  {
    standardGeneric("MeasureSweeps")
  }
)

setMethod("MeasureSweeps",
          "PRecording",
          function(X,
                   Trace,
                   Sweeps = GetSweepNames(X),
                   Time,
                   label,
                   FUN = mean,
                   ReturnPObject = T) {
            if (length(Trace) > 1) {
              stop("This function can only be applied to a single Trace")
            }
            if (as.character(substitute(FUN)) %in% c("+", "-", "+", "/", "^", "**")) {
              message(
                paste(
                  "Binary operator",
                  as.character(substitute(FUN)),
                  "applied. Use Time vector as exclusive points."
                )
              )
              TimeExclusive = T
            } else{
              TimeExclusive = F
            }
            out <- apply(
              GetData(
                X,
                Traces = Trace,
                Time = Time,
                Sweeps = Sweeps,
                nowarnings = T,
                TimeExclusive = TimeExclusive
              ),
              "Time",
              FUN = FUN,
              Verbose = F
            )

            if (!ReturnPObject) {
              rownames(out) <- paste0(label, ".", Sweeps)
              return(out)
            } else{
              return(AddMetaData(X,
                                 out,
                                 title = label,
                                 Verbose = F))
            }
          })

setMethod("MeasureSweeps",
          "PCollection",
          function(X,
                   Trace,
                   Sweeps = GetSweepNames(X),
                   Time,
                   label,
                   FUN = mean,
                   ReturnPObject = T) {
            if (length(Trace) > 1) {
              stop("This function can only be applied to a single Trace")
            }
            X <- lapply(X,
                        function(x) {
                          MeasureSweeps(
                            x,
                            Trace = Trace,
                            Time = Time,
                            Sweeps = Sweeps,
                            label = label,
                            FUN = FUN,
                            ReturnPObject = T
                          )
                        },
                        ReturnPObject = T)

            out <- lapply(X, function(x) {
              GetMetaData(x, label)
            })

            if (length(Sweeps) > 1) {
              label <- paste0(label, ".", Sweeps)
            }
            colnames(out) <- label
            if (!ReturnPObject) {
              out <- as.matrix(out)
              colnames(out) <- label
              rownames(out) <- X@Names
              return(out)
            } else{
              return(AddMetaData(X,
                                 out,
                                 title = label))
            }
          })

#' @describeIn Measure This is a convenience method providing easy to process data.frames for generation of time series and dose-response curves
#' @return MeasureStimResp: A a \link[base:data.frame]{data.frame} with five columns: "Name","Group","Stimulus","StimTimes","Response"
#' @exportMethod MeasureStimResp
setGeneric(
  name = "MeasureStimResp",
  def = function(X,
                 StimTrace = "V-mon",
                 RespTrace = "I-mon",
                 Time,
                 FUN = mean)
  {
    standardGeneric("MeasureStimResp")
  }
)

setMethod("MeasureStimResp",
          "PRecording",
          function(X,
                   StimTrace = "V-mon",
                   RespTrace = "I-mon",
                   Time,
                   FUN = mean) {
            stim <- MeasureSweeps(
              X,
              Trace = StimTrace,
              Sweeps = GetSweepNames(X),
              Time,
              label = "Stimulus",
              FUN = FUN,
              ReturnPObject = F
            )
            resp <- MeasureSweeps(
              X,
              Trace = RespTrace,
              Sweeps = GetSweepNames(X),
              Time,
              label = "Response",
              FUN = FUN,
              ReturnPObject = F
            )
            out <-
              as.data.frame(cbind(stim, GetSweepTimes(X) - min(GetSweepTimes(X)), resp))
            out <- cbind <- cbind(GetSweepNames(X), out)
            colnames(out) <-
              c("Name", "Stimulus", "StimTimes", "Response")
            out
          })

#' @importFrom tidyr pivot_longer
#' @importFrom dplyr left_join
setMethod("MeasureStimResp",
          "PCollection",
          function(X,
                   StimTrace = "V-mon",
                   RespTrace = "I-mon",
                   Time,
                   FUN = mean) {
            stim <- t(
              MeasureSweeps(
                GetData(X, Recordings = GetRecordingNames(X)[1]),
                Trace = StimTrace,
                Sweeps = GetSweepNames(X),
                Time,
                label = "Stimulus",
                FUN = FUN,
                ReturnPObject = F
              )
            )
            resp <- t(
              MeasureSweeps(
                X,
                Trace = RespTrace,
                Sweeps = GetSweepNames(X),
                Time,
                label = "Response",
                FUN = FUN,
                ReturnPObject = F
              )
            )
            out <-
              as.data.frame(cbind(t(stim), GetSweepTimes(X) - min(GetSweepTimes(X)), resp))
            colnames(out) <- c("Stimulus", "StimTimes", X@Names)
            out <- pivot_longer(out, X@Names)
            colnames(out) <-
              c("Stimulus", "StimTimes", "Name", "Response")
            groups <-
              as.data.frame(cbind(X@Names, as.character(X@Group)))
            colnames(groups) <- c("Name", "Group")
            groups$Name <- as.factor(groups$Name)
            groups$Group <- as.factor(groups$Group)
            out <-
              as.data.frame(left_join(out, groups, by = "Name", copy = T))
            out[, c("Name", "Group", "Stimulus", "StimTimes", "Response")]
            out
          })
