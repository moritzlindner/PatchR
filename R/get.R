#' Accession functions
#'
#' These functions are used to access information from \linkS4class{PMRecording} and/or \linkS4class{PMCollection} objects
#'
#' @param object A \linkS4class{PMRecording} or \linkS4class{PMCollection}  object
#' @param which A name of a valid Group in a \linkS4class{PMCollection} (for \code{getGroupMembers}). \cr A name or a vector of names of slot(s) in RecordingParams (for \code{getRecParam}). \cr A name or a vector of names of a column in the MetaData slot (for \code{getMetaData}).
#' @name get
NULL

#' ------------------
setGeneric(name="getSweepNames",
           def=function(object)
           {
             standardGeneric("getSweepNames")
           }
)
#' @rdname get
#' @exportMethod getSweepNames
setMethod("getSweepNames",
          "PMRecording",
          function(object) {
            object@Sweeps
          }
)
#' @rdname get
#' @exportMethod getSweepNames
setMethod("getSweepNames",
          "PMCollection",
          function(object) {
            getSweepNames(object@Series[[1]])
          }
)

#' ------------------
setGeneric(name="getTraceNames",
           def=function(object)
           {
             standardGeneric("getTraceNames")
           }
)
#' @rdname get
#' @exportMethod getTraceNames
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
setGeneric(name="getTimeTrace",
           def=function(object)
           {
             standardGeneric("getTimeTrace")
           }
)
#' @rdname get
#' @exportMethod getTimeTrace
setMethod("getTimeTrace",
          "PMRecording",
          function(object) {
            object@getTimeTrace
          }
)
#' @rdname get
#' @exportMethod getTimeTrace
setMethod("getTimeTrace",
          "PMCollection",
          function(object) {
            getTimeTrace(object@Series[[1]])
          }
)


#' ------------------
setGeneric(name="getCSlow",
           def=function(object)
           {
             standardGeneric("getCSlow")
           }
)
#' @rdname get
#' @exportMethod getCSlow
setMethod("getCSlow",
          "PMRecording",
          function(object) {
            object@RecordingParams@Cs
          }
)
#' @rdname get
#' @exportMethod getCSlow
setMethod("getCSlow",
          "PMCollection",
          function(object) {
            lapply(object,getCSlow)
          }
)

#' ------------------
setGeneric(name="getGroupMembers",
           def=function(object,which)
           {
             standardGeneric("getGroupMembers")
           }
)
#' @rdname get
#' @exportMethod getGroupMembers
setMethod("getGroupMembers",
          "PMCollection",
          function(object,which) {
            object@Names[object@Group %in% which]
          }
)

#' ------------------
setGeneric(name="getRecParam",
           def=function(object,
                        which)
           {
             standardGeneric("getRecParam")
           }
)
#' @rdname get
#' @exportMethod getRecParam
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
#' @rdname get
#' @exportMethod getRecParam
setMethod("getRecParam",
          "PMCollection",
          function(object,which) {
            out<-lapply(object,function(x){unlist(lapply(paste0("x@RecordingParams@",which),function(y){eval(parse(text=y))}))})
            colnames(out)<-which
            out
          }
)

#' ------------------
setGeneric(name="getMetaData",
           def=function(object,which)
           {
             standardGeneric("getMetaData")
           }
)
#' @rdname get
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
#' @rdname get
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

