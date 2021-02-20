#' Accession methods
#'
#' These methods are used to access information from \linkS4class{PRecording} and/or \linkS4class{PCollection} objects
#'
#' @param X A \linkS4class{PRecording} or \linkS4class{PCollection} object
#' @param which A name of a valid Group in a \linkS4class{PCollection} (for \code{GetGroupMembers}). \cr A name or a vector of names of slot(s) in RecordingParams (for \code{GetRecParam}). \cr A name or a vector of names of a column in the MetaData slot (for \code{GetMetaData}).
#' @details These methods can be used to access information stored in \linkS4class{PRecording} and/or \linkS4class{PCollection} objects. \cr \cr
#' @return A numeric vector.
#' @examples
#' GetRecParam(recording,c("Cs","Rs"))
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
setMethod("GetSweepNames",
          "PRecording",
          function(X) {
            X@Sweeps
          }
)
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
setMethod("GetTraceNames",
          "PRecording",
          function(X) {
            X@Traces
          }
)
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
setMethod("GetTimeTrace",
          "PRecording",
          function(X) {
            X@TimeTrace
          }
)
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
setMethod("GetSweepTimes",
          "PRecording",
          function(X) {
            X@SweepTimes
          }
)
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
setMethod("GetCSlow",
          "PRecording",
          function(X) {
            X@RecordingParams@Cs
          }
)
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
setMethod("GetRecParam",
          "PRecording",
          function(X,which) {
            X@RecordingParams[[which]]
          }
)
setMethod("GetRecParam",
          "PRecordingParams",
          function(X,which) {
            X[[which]]
          }
)
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
setMethod("GetGroupMembers",
          "PCollection",
          function(X,which) {
            X@Names[X@Group %in% which]
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
setMethod("GetMetaData",
          "PRecording",
          function(X,which=colnames(X@MetaData)) {
            out<-cbind(GetSweepNames(X),as.data.frame(X@MetaData[,which]))
            colnames(out)<-c("Sweep",which)
            rownames(out)<-GetSweepNames(X)
            out
          }
)
setMethod("GetMetaData",
          "PCollection",
          function(X,which=colnames(X@MetaData)) {
            out<-cbind(X@Names,X@Group,as.data.frame(X@MetaData[,which]))
            colnames(out)<-c("Series","Group",which)
            rownames(out)<-X@Names
            out
          }
)

