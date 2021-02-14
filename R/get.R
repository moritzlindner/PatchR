setGeneric(name="getSweeps",
           def=function(object)
           {
             standardGeneric("getSweeps")
           }
)

#' getSweeps
#'
#' get list of Sweeps from a \link[=PMSeries]{PMSeries} object
#'
#' @param object A \link[=PMSeries]{PMSeries} object
#' @exportMethod getSweeps
setMethod("getSweeps",
          "PMSeries",
          function(object) {
            object@Sweeps
          }
)

setGeneric(name="getTraces",
           def=function(object)
           {
             standardGeneric("getTraces")
           }
)

#' getTraces
#'
#' get list of Traces from a \link[=PMSeries]{PMSeries} or \link[=PMRecordingParams]{PMRecordingParams} object
#'
#' @param object A \link[=PMSeries]{PMSeries} or \link[=PMRecordingParams]{PMRecordingParams} object
#' @exportMethod getTraces
setMethod("getTraces",
          "PMSeries",
          function(object) {
            object@Traces
          }
)

#' @exportMethod getTraces
setMethod("getTraces",
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
#' get Time trace from a \link[=PMSeries]{PMSeries}  object
#'
#' @param object A \link[=PMSeries]{PMSeries}  object
#' @exportMethod getTimeTrace
setMethod("getTimeTrace",
          "PMSeries",
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
#' get Cs from PMSeries
#'
#' @param object A PMSeries object
#' @exportMethod getCs
setMethod("getCs",
          "PMSeries",
          function(object) {
            object@RecordingParams@Cs
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
#' get MetaData from PMSeries or PMExperiment
#'
#' @param object A PMSeries or PMExperiment object
#' @param which columns form MetaData to retrieve. Default is all.
#' @exportMethod getMetaData
setMethod("getMetaData",
          c("PMSeries"),
          function(object,which=colnames(object@MetaData)) {
            out<-cbind(object@Names,object@Group,as.data.frame(object@MetaData[,which]))
            colnames(out)<-c("Series","Group",which)
            rownames(out)<-object@Names
            out
          }
)

#' @exportMethod getMetaData
setMethod("getMetaData",
          c("PMExperiment"),
          function(object,which=colnames(object@MetaData)) {
            out<-cbind(object@Names,object@Group,as.data.frame(object@MetaData[,which]))
            colnames(out)<-c("Series","Group",which)
            rownames(out)<-object@Names
            out
          }
)

