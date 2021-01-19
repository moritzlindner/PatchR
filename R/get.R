setGeneric(name="getSweeps",
           def=function(object)
           {
             standardGeneric("getSweeps")
           }
)

#' getSweeps
#'
#' get list of Sweeps from a PMSeries object
#'
#' @param object A PMSeries object
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
#' get list of Traces from a PMSeries object
#'
#' @param object A PMSeries or PMRecordingParams object
#' @exportMethod getTraces
setMethod("getTraces",
          "PMSeries",
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
#' get Time trace from a PMSeries object
#'
#' @param object A PMSeries object
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
