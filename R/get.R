setGeneric(name="getSweeps",
           def=function(object)
           {
             standardGeneric("getSweeps")
           }
)

#' getSweeps
#'
#' get list of Sweeps from a PMTrace object
#'
#' @param object A PMTrace object
#' @exportMethod getSweeps
setMethod("getSweeps",
          "PMTrace",
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
#' get list of Traces from a PMTrace object
#'
#' @param object A PMTrace or PMRecordingParams object
#' @exportMethod getTraces
setMethod("getTraces",
          "PMTrace",
          function(object) {
            object@Traces
          }
)

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
#' get Time trace from a PMTrace object
#'
#' @param object A PMTrace object
#' @exportMethod getTimeTrace
setMethod("getTimeTrace",
          "PMTrace",
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
#' get Cs from PMTrace
#'
#' @param object A PMTrace object
#' @exportMethod getCs
setMethod("getCs",
          "PMTrace",
          function(object) {
            object@RecordingParams@Cs
          }
)
