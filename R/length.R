#' (OK) Length of an object
#'
#' `r lifecycle::badge("stable")` \cr
#' returns the number of Series/Recordings in a \linkS4class{PCollection} or the number of Sweeps from a \linkS4class{PRecording}
#'
#' @param x a \var{PRecording} or \var{PCollection} object.
#' @return The number of Series/Recordings in a \linkS4class{PCollection} or the number of Sweeps from a \linkS4class{PRecording} as a one-dimensional \var{numeric} vector.
#' @name length
#' @exportMethod length
NULL

#' @describeIn length Method for PRecording
setMethod("length",
          "PRecording",
          function(x){
            length(x@Sweeps)
          }
)

#' @noMd
setMethod("length",
          "PCollection",
          function(x){
            length(x@Series)
          }
)
