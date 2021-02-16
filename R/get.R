#' Accession methods
#'
#' These methods are used to access information from \linkS4class{PMRecording} and/or \linkS4class{PMCollection} objects
#'
#' @param object A \linkS4class{PMRecording} or \linkS4class{PMCollection}  object
#' @param which A name of a valid Group in a \linkS4class{PMCollection} (for \code{getGroupMembers}). \cr A name or a vector of names of slot(s) in RecordingParams (for \code{getRecParam}). \cr A name or a vector of names of a column in the MetaData slot (for \code{getMetaData}).
#' @details These methods can be used to access information stored in \linkS4class{PMRecording} and/or \linkS4class{PMCollection} objects. \cr \cr
#' @return A numeric vector or a matrix (for \code{getMetaData}).
#' @examples
#' getRecParam(recording,c("Cs","Rs"))
#' @name get
NULL

#' ------------------
#' @describeIn get Can be used on \linkS4class{PMRecording} and \linkS4class{PMCollection} objects and return the Sweep names.
#' @exportMethod getSweepNames
setGeneric(name="getSweepNames",
           def=function(object)
           {
             standardGeneric("getSweepNames")
           }
)
setMethod("getSweepNames",
          "PMRecording",
          function(object) {
            object@Sweeps
          }
)
setMethod("getSweepNames",
          "PMCollection",
          function(object) {
            getSweepNames(object@Series[[1]])
          }
)

#' ------------------
#' @describeIn get Can be used on \linkS4class{PMRecording} and \linkS4class{PMCollection} objects and return the Trace names.
#' @exportMethod getTraceNames
setGeneric(name="getTraceNames",
           def=function(object)
           {
             standardGeneric("getTraceNames")
           }
)
setMethod("getTraceNames",
          "PMRecording",
          function(object) {
            object@Traces
          }
)
setMethod("getTraceNames",
          "PMRecordingParams",
          function(object) {
            object@Traces
          }
)
#' ------------------
#' @describeIn get Can be used on \linkS4class{PMRecording} and \linkS4class{PMCollection} objects and returns a vector containing the times at which the individual sweeps were recorded.

setGeneric(name="getTimeTrace",
           def=function(object)
           {
             standardGeneric("getTimeTrace")
           }
)
#' @exportMethod getTimeTrace
setMethod("getTimeTrace",
          "PMRecording",
          function(object) {
            object@TimeTrace
          }
)
#' @exportMethod getTimeTrace
setMethod("getTimeTrace",
          "PMCollection",
          function(object) {
            getTimeTrace(object@Series[[1]])
          }
)
#' ------------------
#' @describeIn get is a special case of \code{getRecParam} and can be used on \linkS4class{PMRecording} and \linkS4class{PMCollection} objects. It returns the C-slow value.
#' @exportMethod getCSlow
setGeneric(name="getCSlow",
           def=function(object)
           {
             standardGeneric("getCSlow")
           }
)

setMethod("getCSlow",
          "PMRecording",
          function(object) {
            object@RecordingParams@Cs
          }
)
#' @describeIn get
#' @exportMethod getCSlow
setMethod("getCSlow",
          "PMCollection",
          function(object) {
            lapply(object,getCSlow)
          }
)

#' ------------------
#' @describeIn get Can be used on \linkS4class{PMRecording} and \linkS4class{PMCollection} objects and returns any value stored in the RecordingParams slot.
#' @exportMethod getRecParam
setGeneric(name="getRecParam",
           def=function(object,
                        which)
           {
             standardGeneric("getRecParam")
           }
)
setMethod("getRecParam",
          "PMRecording",
          function(object,which) {
            object@RecordingParams[[which]]
          }
)
setMethod("getRecParam",
          "PMRecordingParams",
          function(object,which) {
            object[[which]]
          }
)
setMethod("getRecParam",
          "PMCollection",
          function(object,which) {
            out<-lapply(object,function(x){unlist(lapply(paste0("x@RecordingParams@",which),function(y){eval(parse(text=y))}))})
            colnames(out)<-which
            out
          }
)
#' ------------------
#' @describeIn get get Can be used on \linkS4class{PMCollection} objects and returns the names of all PMRecordings belonging to the named Group.
#' @exportMethod getGroupMembers
setGeneric(name="getGroupMembers",
           def=function(object,which)
           {
             standardGeneric("getGroupMembers")
           }
)
setMethod("getGroupMembers",
          "PMCollection",
          function(object,which) {
            object@Names[object@Group %in% which]
          }
)
#' ------------------
#' @describeIn get Can be used on \linkS4class{PMRecording} and \linkS4class{PMCollection} objects and returns one or more columns from the MetaData Slot.
#' @exportMethod getMetaData
setGeneric(name="getMetaData",
           def=function(object,which)
           {
             standardGeneric("getMetaData")
           }
)
setMethod("getMetaData",
          c("PMRecording"),
          function(object,which=colnames(object@MetaData)) {
            out<-cbind(object@Names,object@Group,as.data.frame(object@MetaData[,which]))
            colnames(out)<-c("Series","Group",which)
            rownames(out)<-object@Names
            out
          }
)
setMethod("getMetaData",
          c("PMCollection"),
          function(object,which=colnames(object@MetaData)) {
            out<-cbind(object@Names,object@Group,as.data.frame(object@MetaData[,which]))
            colnames(out)<-c("Series","Group",which)
            rownames(out)<-object@Names
            out
          }
)

