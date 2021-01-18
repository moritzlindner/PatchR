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

setGeneric(name="getChannels",
           def=function(object)
           {
             standardGeneric("getChannels")
           }
)

#' getChannels
#'
#' get list of Channels from a PMTrace object
#'
#' @param object A PMTrace object
#' @exportMethod getChannels
setMethod("getChannels",
          "PMTrace",
          function(object) {
            object@Channels
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

