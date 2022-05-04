#' @describeIn Get This function subsets objects by \var{Trace}, \var{Sweep} or \var{Time}. For  \linkS4class{PCollection} additionally by \var{Recordings} or \var{Group}
#' @param Traces,Sweeps List of traces/channels or sweeps to keep
#' @param Time either a range of time points to keep, or, if \code{TimeExclusive} is \code{TRUE}, then two particular time points
#' @param Recordings Subset by series/recordings. Understands names (= file names of the recordings) or indices or by logical indexing. Only for \linkS4class{PCollection} .
#' @param Group Subset by Group name. Only for  \linkS4class{PCollection} .
#' @param TimeExclusive Keep only the two time points stated under Time, not the range
#' @param nowarnings Supress warning messages.
#' @return A \linkS4class{PRecording} or \linkS4class{PCollection} object (for \code{GetData})
#' @exportMethod GetData
setGeneric(
  name = "GetData",
  def = function(X,
                 Traces = GetTraceNames(X),
                 Sweeps = GetSweepNames(X),
                 Time = range(GetTimeTrace(X)),
                 Recordings = NULL,
                 Group = NULL,
                 TimeExclusive = F,
                 nowarnings = F)
  {
    standardGeneric("GetData")
  }
)

setMethod("GetData",
          "PRecording",
          function(X,
                   Traces = GetTraceNames(X),
                   Sweeps = GetSweepNames(X),
                   Time = range(GetTimeTrace(X)),
                   TimeExclusive = F,
                   nowarnings = F)
          {
            if (!nowarnings) {
              if (!(length(X@Plots) == 0 & all(dim(X@MetaData) == 0)))
              {
                warning("Subsetting clears all metadata and plotting slots for data consistency!")
              }
            }
            if (isFALSE(all.equal(Traces, GetTraceNames(X)))) {
              if (!nowarnings) {
                cat("Only keep Traces:", Traces, "\n")
              }
              if (!all(Traces %in% GetTraceNames(X))) {
                stop("Traces to subset not in X")
              }
            }
            if (isFALSE(all.equal(Sweeps, GetSweepNames(X)))) {
              if (!nowarnings) {
                cat("Only keep Sweeps: ", Sweeps, "\n")
              }
              if (!all(Sweeps %in% GetSweepNames(X))) {
                stop("Traces to subset not in X")
              }
            }
            if (!isTRUE(all.equal(Time, range(GetTimeTrace(X))))) {
              if (!TimeExclusive) {
                if (!nowarnings) {
                  cat("Only keep Times: ", Time[1], " to ", Time[2], "\n")
                }
                Time <-
                  GetTimeTrace(X)[GetTimeTrace(X) >= Time[1] &
                                    GetTimeTrace(X) <= Time[2]]
              } else{
                # if extracting exact time points. get closest to values entered
                Time[1] <-
                  GetTimeTrace(X)[which(abs(GetTimeTrace(X) - Time[1]) == min(abs(GetTimeTrace(X) -
                                                                                    Time[1])))]
                Time[2] <-
                  GetTimeTrace(X)[which(abs(GetTimeTrace(X) - Time[2]) == min(abs(GetTimeTrace(X) -
                                                                                    Time[2])))]
                cat("Only keep Times: ",
                    Time[1],
                    " and ",
                    length(Time) - 1,
                    "others \n")
              }
            } else{
              Time <- GetTimeTrace(X)
            }

            RecordingParams <- X@RecordingParams
            RecordingParams@Traces <-
              RecordingParams@Traces[RecordingParams@Traces %in% Traces]
            DATA <- list()
            for (i in Traces) {
              DATA[[i]] <-
                as.matrix(X@Data[[i]][GetTimeTrace(X) %in% Time, GetSweepNames(X) %in% Sweeps])
            }
            PRecording(
              Traces = GetTraceNames(X)[GetTraceNames(X) %in% Traces],
              Units = X@Units[GetTraceNames(X) %in% Traces],
              TimeTrace = GetTimeTrace(X)[GetTimeTrace(X) %in% Time],
              TimeUnit = X@TimeUnit,
              Sweeps = GetSweepNames(X)[GetSweepNames(X) %in% Sweeps],
              SweepTimes = X@SweepTimes[GetSweepNames(X) %in% Sweeps],
              Data = DATA,
              RecordingParams = RecordingParams
            )
          })

setMethod("GetData",
          "PCollection",
          function(X,
                   Traces = GetTraceNames(X),
                   Sweeps = GetSweepNames(X),
                   Time = range(GetTimeTrace(X)),
                   Recordings = GetRecordingNames(X),
                   Group = GetGroupNames(X),
                   TimeExclusive = F,
                   nowarnings = F)
          {
            X <-
              lapply(X, function(x)
                GetData(x, Traces, Sweeps, Time, TimeExclusive, nowarnings = nowarnings), ReturnPObject =
                  T)

            if (all.equal(Group, GetGroupNames(X)) != TRUE) {
              if (!nowarnings) {
                warning("Plots dropped for consistency.")
              }
              keep <- as.character(X@Group) %in% as.character(Group)
              X <- PCollection(
                Recordings = X@Recordings[keep],
                Names = X@Names[keep],
                Group = X@Group[keep],
                MetaData = X@MetaData[keep],
                RecordingParams = X@RecordingParams
              )
            }

            if (all.equal(Recordings, GetRecordingNames(X)) != TRUE) {
              if (!nowarnings) {
                warning("Plots dropped for consistency.")
              }
              if (is.character(Recordings)) {
                keep <- GetRecordingNames(X) %in% Recordings
              }
              if (is.numeric(Recordings)) {
                keep <- logical(length(X@Recordings))
                keep[Recordings] <- TRUE
              }
              md <- matrix(nrow = 0, ncol = 0)
              if (sum(keep) > 1) {
                try(md <- X@MetaData[keep, ], silent = T)
                X <- PCollection(
                  Recordings = X@Recordings[keep],
                  Names = X@Names[keep],
                  Group = X@Group[keep],
                  MetaData = md,
                  RecordingParams = X@RecordingParams
                )
              } else{
                X <- X@Recordings[[which(keep)]]
              }
            }
            X
          })


#' @describeIn Get Subset is an alias of Getdata
Subset <- GetData