#' Accession methods
#'
#' These methods are used to access information from \linkS4class{PRecording} and/or \linkS4class{PCollection} objects
#'
#' @param object A \linkS4class{PRecording} or \linkS4class{PCollection}  object
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
           def=function(object)
           {
             standardGeneric("getSweepNames")
           }
)
setMethod("getSweepNames",
          "PRecording",
          function(object) {
            object@Sweeps
          }
)
setMethod("getSweepNames",
          "PCollection",
          function(object) {
            getSweepNames(object@Series[[1]])
          }
)

#' ------------------
#' @describeIn get Can be used on \linkS4class{PRecording} and \linkS4class{PCollection} objects and return the Trace names.
#' @exportMethod getTraceNames
setGeneric(name="getTraceNames",
           def=function(object)
           {
             standardGeneric("getTraceNames")
           }
)
setMethod("getTraceNames",
          "PRecording",
          function(object) {
            object@Traces
          }
)
setMethod("getTraceNames",
          "PRecordingParams",
          function(object) {
            object@Traces
          }
)
#' ------------------
#' @describeIn get Can be used on \linkS4class{PRecording} and \linkS4class{PCollection} objects and returns a vector containing the times at which the individual sweeps were recorded.
#' @exportMethod getTimeTrace
setGeneric(name="getTimeTrace",
           def=function(object)
           {
             standardGeneric("getTimeTrace")
           }
)
setMethod("getTimeTrace",
          "PRecording",
          function(object) {
            object@TimeTrace
          }
)
setMethod("getTimeTrace",
          "PCollection",
          function(object) {
            getTimeTrace(object@Series[[1]])
          }
)
#' ------------------
#' @describeIn get Can be used on \linkS4class{PRecording} and \linkS4class{PCollection} objects and returns a vector containing the times at which the individual sweeps were recorded.
#' @exportMethod getSweepTimes
setGeneric(name="getSweepTimes",
           def=function(object)
           {
             standardGeneric("getSweepTimes")
           }
)
setMethod("getSweepTimes",
          "PRecording",
          function(object) {
            object@SweepTimes
          }
)
setMethod("getSweepTimes",
          "PCollection",
          function(object) {
            getSweepTimes(object@Series[[1]])
          }
)
#' ------------------
#' @describeIn get is a special case of \code{getRecParam} and can be used on \linkS4class{PRecording} and \linkS4class{PCollection} objects. It returns the C-slow value.
#' @exportMethod getCSlow
setGeneric(name="getCSlow",
           def=function(object)
           {
             standardGeneric("getCSlow")
           }
)
setMethod("getCSlow",
          "PRecording",
          function(object) {
            object@RecordingParams@Cs
          }
)
#' @describeIn get
setMethod("getCSlow",
          "PCollection",
          function(object) {
            lapply(object,getCSlow)
          }
)

#' ------------------
#' @describeIn get Can be used on \linkS4class{PRecording} and \linkS4class{PCollection} objects and returns any value stored in the RecordingParams slot.
#' @exportMethod getRecParam
setGeneric(name="getRecParam",
           def=function(object,
                        which)
           {
             standardGeneric("getRecParam")
           }
)
setMethod("getRecParam",
          "PRecording",
          function(object,which) {
            object@RecordingParams[[which]]
          }
)
setMethod("getRecParam",
          "PRecordingParams",
          function(object,which) {
            object[[which]]
          }
)
setMethod("getRecParam",
          "PCollection",
          function(object,which) {
            out<-lapply(object,function(x){unlist(lapply(paste0("x@RecordingParams@",which),function(y){eval(parse(text=y))}))})
            colnames(out)<-which
            out
          }
)
#' ------------------
#' @describeIn get get Can be used on \linkS4class{PCollection} objects and returns the names of all PRecordings belonging to the named Group.
#' @exportMethod getGroupMembers
setGeneric(name="getGroupMembers",
           def=function(object,which)
           {
             standardGeneric("getGroupMembers")
           }
)
setMethod("getGroupMembers",
          "PCollection",
          function(object,which) {
            object@Names[object@Group %in% which]
          }
)
#' ------------------
#' @describeIn get Can be used on \linkS4class{PRecording} and \linkS4class{PCollection} objects and returns one or more columns from the MetaData Slot.
#' @exportMethod getMetaData
setGeneric(name="getMetaData",
           def=function(object,which)
           {
             standardGeneric("getMetaData")
           }
)
setMethod("getMetaData",
          c("PRecording"),
          function(object,which=colnames(object@MetaData)) {
            out<-cbind(getSweepNames(object),as.data.frame(object@MetaData[,which]))
            colnames(out)<-c("Sweep",which)
            rownames(out)<-getSweepNames(object)
            out
          }
)
setMethod("getMetaData",
          c("PCollection"),
          function(object,which=colnames(object@MetaData)) {
            out<-cbind(object@Names,object@Group,as.data.frame(object@MetaData[,which]))
            colnames(out)<-c("Series","Group",which)
            rownames(out)<-object@Names
            out
          }
)

