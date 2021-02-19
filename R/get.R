#' Accession methods
#'
#' These methods are used to access information from \linkS4class{PRecording} and/or \linkS4class{PCollection} objects
#'
#' @param X A \linkS4class{PRecording} or \linkS4class{PCollection} object
#' @param which A name of a valid Group in a \linkS4class{PCollection} (for \code{getGroupMembers}). \cr A name or a vector of names of slot(s) in RecordingParams (for \code{getRecParam}). \cr A name or a vector of names of a column in the MetaData slot (for \code{getMetaData}).
#' @details These methods can be used to access information stored in \linkS4class{PRecording} and/or \linkS4class{PCollection} objects. \cr \cr
#' @return A numeric vector or a matrix (for \code{getMetaData}).
#' @examples
#' getRecParam(recording,c("Cs","Rs"))
#' @name get
NULL

#' ------------------
#' @describeIn get Can be used on \linkS4class{PRecording} and \linkS4class{PCollection} objects and return the Sweep names.
#' @exportMethod getSweepNames
setGeneric(name="getSweepNames",
           def=function(X)
           {
             standardGeneric("getSweepNames")
           }
)
setMethod("getSweepNames",
          "PRecording",
          function(X) {
            X@Sweeps
          }
)
setMethod("getSweepNames",
          "PCollection",
          function(X) {
            getSweepNames(X@Series[[1]])
          }
)

#' ------------------
#' @describeIn get Can be used on \linkS4class{PRecording} and \linkS4class{PCollection} objects and return the Trace names.
#' @exportMethod getTraceNames
setGeneric(name="getTraceNames",
           def=function(X)
           {
             standardGeneric("getTraceNames")
           }
)
setMethod("getTraceNames",
          "PRecording",
          function(X) {
            X@Traces
          }
)
setMethod("getTraceNames",
          "PRecordingParams",
          function(X) {
            X@Traces
          }
)
#' ------------------
#' @describeIn get Can be used on \linkS4class{PRecording} and \linkS4class{PCollection} objects and returns a vector containing the times at which the individual sweeps were recorded.
#' @exportMethod getTimeTrace
setGeneric(name="getTimeTrace",
           def=function(X)
           {
             standardGeneric("getTimeTrace")
           }
)
setMethod("getTimeTrace",
          "PRecording",
          function(X) {
            X@TimeTrace
          }
)
setMethod("getTimeTrace",
          "PCollection",
          function(X) {
            getTimeTrace(X@Series[[1]])
          }
)
#' ------------------
#' @describeIn get Can be used on \linkS4class{PRecording} and \linkS4class{PCollection} objects and returns a vector containing the times at which the individual sweeps were recorded.
#' @exportMethod getSweepTimes
setGeneric(name="getSweepTimes",
           def=function(X)
           {
             standardGeneric("getSweepTimes")
           }
)
setMethod("getSweepTimes",
          "PRecording",
          function(X) {
            X@SweepTimes
          }
)
setMethod("getSweepTimes",
          "PCollection",
          function(X) {
            getSweepTimes(X@Series[[1]])
          }
)
#' ------------------
#' @describeIn get is a special case of \code{getRecParam} and can be used on \linkS4class{PRecording} and \linkS4class{PCollection} objects. It returns the C-slow value.
#' @exportMethod getCSlow
setGeneric(name="getCSlow",
           def=function(X)
           {
             standardGeneric("getCSlow")
           }
)
setMethod("getCSlow",
          "PRecording",
          function(X) {
            X@RecordingParams@Cs
          }
)
#' @describeIn get
setMethod("getCSlow",
          "PCollection",
          function(X) {
            lapply(X,getCSlow)
          }
)

#' ------------------
#' @describeIn get Can be used on \linkS4class{PRecording} and \linkS4class{PCollection} objects and returns any value stored in the RecordingParams slot.
#' @exportMethod getRecParam
setGeneric(name="getRecParam",
           def=function(X,
                        which)
           {
             standardGeneric("getRecParam")
           }
)
setMethod("getRecParam",
          "PRecording",
          function(X,which) {
            X@RecordingParams[[which]]
          }
)
setMethod("getRecParam",
          "PRecordingParams",
          function(X,which) {
            X[[which]]
          }
)
setMethod("getRecParam",
          "PCollection",
          function(X,which) {
            out<-lapply(X,function(x){unlist(lapply(paste0("x@RecordingParams@",which),function(y){eval(parse(text=y))}))})
            colnames(out)<-which
            out
          }
)
#' ------------------
#' @describeIn get get Can be used on \linkS4class{PCollection} objects and returns the names of all PRecordings belonging to the named Group.
#' @exportMethod getGroupMembers
setGeneric(name="getGroupMembers",
           def=function(X,which)
           {
             standardGeneric("getGroupMembers")
           }
)
setMethod("getGroupMembers",
          "PCollection",
          function(X,which) {
            X@Names[X@Group %in% which]
          }
)
#' ------------------
#' @describeIn get Can be used on \linkS4class{PRecording} and \linkS4class{PCollection} objects and returns one or more columns from the MetaData Slot.
#' @exportMethod getMetaData
setGeneric(name="getMetaData",
           def=function(X,which)
           {
             standardGeneric("getMetaData")
           }
)
setMethod("getMetaData",
          "PRecording",
          function(X,which=colnames(X@MetaData)) {
            out<-cbind(getSweepNames(X),as.data.frame(X@MetaData[,which]))
            colnames(out)<-c("Sweep",which)
            rownames(out)<-getSweepNames(X)
            out
          }
)
setMethod("getMetaData",
          "PCollection",
          function(X,which=colnames(X@MetaData)) {
            out<-cbind(X@Names,X@Group,as.data.frame(X@MetaData[,which]))
            colnames(out)<-c("Series","Group",which)
            rownames(out)<-X@Names
            out
          }
)

