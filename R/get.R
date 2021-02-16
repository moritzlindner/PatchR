#' Accession functions
#'
#' These functions are used to access information from \link[=PMRecording]{PMRecording} and  \link[=PMCollection]{PMCollection} objects
#'
#' @param objectA  \link[=PMRecording]{PMRecording} or PMCollection object
#' @name accession
NULL

setGeneric(name="SweepNames",
           def=function(object)
           {
             standardGeneric("SweepNames")
           }
)

#' @rdname accession
#' @exportMethod SweepNames
setMethod("SweepNames",
          "PMRecording",
          function(object) {
            object@Sweeps
          }
)

#' @exportMethod SweepNames
setMethod("SweepNames",
          "PMCollection",
          function(object) {
            SweepNames(object@Series[[1]])
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
#' @inheritParams SweepNames
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
#' @inheritParams SweepNames
#' @exportMethod getCs
setMethod("getCs",
          "PMRecording",
          function(object) {
            object@RecordingParams@Cs
          }
)

setGeneric(name="getGroupMembers",
           def=function(object,Group)
           {
             standardGeneric("getGroupMembers")
           }
)

#' getGroupMembers
#'
#' get names of all memebers of a group from PMCollection
#'
#' @param object A PMCollection object
#' @param Goup one or more name(s) of Groups contained in PMCollection
#' @exportMethod getGroupMembers
setMethod("getGroupMembers",
          "PMCollection",
          function(object,Group) {
            object@Names[object@Group %in% Group]
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
#' @inheritParams SweepNames
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
            out<-lapply(object,function(x){unlist(lapply(paste0("x@RecordingParams@",param),function(y){eval(parse(text=y))}))})
            colnames(out)<-param
            out
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

