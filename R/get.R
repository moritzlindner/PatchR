setGeneric(name="SweepNames",
           def=function(object)
           {
             standardGeneric("SweepNames")
           }
)

#' SweepNames
#'
#' get list of Sweeps from a \link[=PMRecording]{PMRecording} object
#'
#' @param object A \link[=PMRecording]{PMRecording} object
#' @exportMethod SweepNames
setMethod("SweepNames",
          "PMRecording",
          function(object) {
            object@Sweeps
          }
)

setGeneric(name="TraceNames",
           def=function(object)
           {
             standardGeneric("TraceNames")
           }
)

#' TraceNames
#'
#' get list of Traces from a \link[=PMRecording]{PMRecording} or \link[=PMRecordingParams]{PMRecordingParams} object
#'
#' @param object A \link[=PMRecording]{PMRecording} or \link[=PMRecordingParams]{PMRecordingParams} object
#' @exportMethod TraceNames
setMethod("TraceNames",
          "PMRecording",
          function(object) {
            object@Traces
          }
)

#' @exportMethod TraceNames
setMethod("TraceNames",
          "PMRecordingParams",
          function(object) {
            object@Traces
          }
)

setGeneric(name="getTimeTrace",
           def=function(object)
           {
             standardGeneric("getTimeTrace")
           }
)

#' getTimeTrace
#'
#' get Time trace from a \link[=PMRecording]{PMRecording}  object
#'
#' @param object A \link[=PMRecording]{PMRecording}  object
#' @exportMethod getTimeTrace
setMethod("getTimeTrace",
          "PMRecording",
          function(object) {
            object@TimeTrace
          }
)

setGeneric(name="getCs",
           def=function(object)
           {
             standardGeneric("getCs")
           }
)

#' getCs
#'
#' get Cs from PMRecording
#'
#' @param object A PMRecording object
#' @exportMethod getCs
setMethod("getCs",
          "PMRecording",
          function(object) {
            object@RecordingParams@Cs
          }
)


setGeneric(name="RecParam",
           def=function(object,
                        param)
           {
             standardGeneric("RecParam")
           }
)

#' RecParam
#'
#' get Recording parameters from PMRecording or PMCollection
#'
#' @param object A PMRecordingor PMCollection object
#' @param param parameter to fetch, can be either of Rpip, RSeal, Urest, Cs, Rs
#' @exportMethod RecParam
setMethod("RecParam",
          "PMRecording",
          function(object,param) {
            object@RecordingParams[[param]]
          }
)

#' @exportMethod RecParam
setMethod("RecParam",
          "PMRecordingParams",
          function(object,param) {
            object[[param]]
          }
)

#' @exportMethod RecParam
setMethod("RecParam",
          "PMCollection",
          function(object,param) {
            lapply(object, function (x) x@RecordingParams[[param]])
          }
)


setGeneric(name="getMetaData",
           def=function(object,which)
           {
             standardGeneric("getMetaData")
           }
)

#' getMetaData
#'
#' get MetaData from PMRecording or PMCollection
#'
#' @param object A PMRecording or PMCollection object
#' @param which columns form MetaData to retrieve. Default is all.
#' @exportMethod getMetaData
setMethod("getMetaData",
          c("PMRecording"),
          function(object,which=colnames(object@MetaData)) {
            out<-cbind(object@Names,object@Group,as.data.frame(object@MetaData[,which]))
            colnames(out)<-c("Series","Group",which)
            rownames(out)<-object@Names
            out
          }
)

#' @exportMethod getMetaData
setMethod("getMetaData",
          c("PMCollection"),
          function(object,which=colnames(object@MetaData)) {
            out<-cbind(object@Names,object@Group,as.data.frame(object@MetaData[,which]))
            colnames(out)<-c("Series","Group",which)
            rownames(out)<-object@Names
            out
          }
)

