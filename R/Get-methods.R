#' (OK) Accession methods
#'
#' `r lifecycle::badge("stable")` \cr
#' These methods are used to access information from \linkS4class{PRecording} and/or \linkS4class{PCollection} objects
#'
#' @param X A \linkS4class{PRecording} or \linkS4class{PCollection} object
#' @param which 1) A name of a valid Group in a \linkS4class{PCollection} (for \code{GetGroupMembers}). \cr 2) A name or a vector of names of slot(s) in RecordingParams (for \code{GetRecParam}). \cr or 3) A name or a vector of names of a column in the MetaData slot (for \code{GetMetaData}).
#' @details These methods can be used to access information stored in \linkS4class{PRecording} and/or \linkS4class{PCollection} objects. \cr \cr
#' @return A numeric vector.
#' @examples
#' \dontrun{
#' GetRecParam(recording,c("Cs","Rs"))
#' }
#' @name Get
NULL

#' ------------------
#' @describeIn Get Returns the Sweep names.
#' @exportMethod GetSweepNames
setGeneric(name="GetSweepNames",
           def=function(X)
           {
             standardGeneric("GetSweepNames")
           }
)

#' @noMd
setMethod("GetSweepNames",
          "PRecording",
          function(X) {
            X@Sweeps
          }
)

#' @noMd
setMethod("GetSweepNames",
          "PCollection",
          function(X) {
            GetSweepNames(X@Series[[1]])
          }
)

#' ------------------
#' @describeIn Get Return the Trace names.
#' @exportMethod GetTraceNames
setGeneric(name="GetTraceNames",
           def=function(X)
           {
             standardGeneric("GetTraceNames")
           }
)
#' @noMd
setMethod("GetTraceNames",
          "PRecording",
          function(X) {
            X@Traces
          }
)
#' @noMd
setMethod("GetTraceNames",
          "PRecordingParams",
          function(X) {
            X@Traces
          }
)
#' ------------------
#' @describeIn Get Returns a vector containing the times at which the individual sweeps were recorded.
#' @exportMethod GetTimeTrace
setGeneric(name="GetTimeTrace",
           def=function(X)
           {
             standardGeneric("GetTimeTrace")
           }
)
#' @noMd
setMethod("GetTimeTrace",
          "PRecording",
          function(X) {
            X@TimeTrace
          }
)
#' @noMd
setMethod("GetTimeTrace",
          "PCollection",
          function(X) {
            GetTimeTrace(X@Series[[1]])
          }
)
#' ------------------
#' @describeIn Get Returns a vector containing the times at which the individual sweeps were recorded.
#' @exportMethod GetSweepTimes
setGeneric(name="GetSweepTimes",
           def=function(X)
           {
             standardGeneric("GetSweepTimes")
           }
)
#' @noMd
setMethod("GetSweepTimes",
          "PRecording",
          function(X) {
            X@SweepTimes
          }
)
#' @noMd
setMethod("GetSweepTimes",
          "PCollection",
          function(X) {
            GetSweepTimes(X@Series[[1]])
          }
)
#' ------------------
#' @describeIn Get This is a special case of \code{GetRecParam} and returns the C-slow value.
#' @exportMethod GetCSlow
setGeneric(name="GetCSlow",
           def=function(X)
           {
             standardGeneric("GetCSlow")
           }
)
#' @noMd
setMethod("GetCSlow",
          "PRecording",
          function(X) {
            X@RecordingParams@Cs
          }
)
#' @noMd
setMethod("GetCSlow",
          "PCollection",
          function(X) {
            lapply(X,GetCSlow)
          }
)

#' ------------------
#' @describeIn Get Returns any value stored in the RecordingParams slot.
#' @exportMethod GetRecParam
setGeneric(name="GetRecParam",
           def=function(X,
                        which)
           {
             standardGeneric("GetRecParam")
           }
)
#' @noMd
setMethod("GetRecParam",
          "PRecording",
          function(X,which) {
            methods::slot(X@RecordingParams,which)
          }
)
#' @noMd
setMethod("GetRecParam",
          "PRecordingParams",
          function(X,which) {
            X[[which]]
          }
)
#' @noMd
setMethod("GetRecParam",
          "PCollection",
          function(X,which) {
            out<-lapply(X,function(x){unlist(lapply(paste0("x@RecordingParams@",which),function(y){eval(parse(text=y))}))})
            colnames(out)<-which
            out
          }
)
#' ------------------
#' @describeIn Get Can be used on \linkS4class{PCollection} objects only and returns the names of all PRecordings belonging to the named Group.
#' @exportMethod GetGroupMembers
setGeneric(name="GetGroupMembers",
           def=function(X,which)
           {
             standardGeneric("GetGroupMembers")
           }
)
#' @noMd
setMethod("GetGroupMembers",
          "PCollection",
          function(X,which) {
            X@Names[X@Group %in% which]
          }
)
#' ------------------
#' @describeIn Get Can be used on \linkS4class{PCollection} objects only and returns the names of all groups.
#' @exportMethod GetGroupNames
setGeneric(name="GetGroupNames",
           def=function(X)
           {
             standardGeneric("GetGroupNames")
           }
)
#' @noMd
setMethod("GetGroupNames",
          "PCollection",
          function(X) {
            levels(X@Group)
          }
)
#' ------------------
#' @describeIn Get Can be used on \linkS4class{PCollection} objects only and returns the names of all series
#' @exportMethod GetSeriesNames
setGeneric(name="GetSeriesNames",
           def=function(X)
           {
             standardGeneric("GetSeriesNames")
           }
)
#' @noMd
setMethod("GetSeriesNames",
          "PCollection",
          function(X) {
            snames<-NULL
            for (i in 1:length(X@Series)){
              snames[i]<-GetRecParam(X@Series[[i]], "Filename")
            }
            snames
          }
)

#' @noMd
setMethod("names",
          "PCollection",
          function(x) {
            GetSeriesNames(x)
          }
)
#' ------------------
#' @describeIn Get Returns one or more columns from the MetaData Slot.
#' @return For \code{GetMetaData} a \code{vector} of type \code{numeric}  or a \code{matrix}.
#' @exportMethod GetMetaData
setGeneric(name="GetMetaData",
           def=function(X,which)
           {
             standardGeneric("GetMetaData")
           }
)
#' @noMd
setMethod("GetMetaData",
          "PRecording",
          function(X,which=colnames(X@MetaData)) {
            out<-cbind(GetSweepNames(X),as.data.frame(X@MetaData[,which]))
            colnames(out)<-c("Sweep",which)
            rownames(out)<-GetSweepNames(X)
            out
          }
)
#' @noMd
setMethod("GetMetaData",
          "PCollection",
          function(X,which=colnames(X@MetaData)) {
            out<-cbind(X@Names,X@Group,as.data.frame(X@MetaData[,which]))
            colnames(out)<-c("Series","Group",which)
            rownames(out)<-X@Names
            out
          }
)

