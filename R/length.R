#' Length of an object
#'
#' `r lifecycle::badge("stable")` \cr
#' Returns the number of recordings in a \linkS4class{PCollection} or the number of sweeps from a \linkS4class{PRecording}
#'
#' @param x a \var{PRecording} or \var{PCollection} object.
#' @return The number of recordings in a \linkS4class{PCollection} or the number of sweeps from a \linkS4class{PRecording} as a one-dimensional \var{numeric} vector.
#' @name length
#' @exportMethod length
NULL

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
            length(x@Recordings)
          }
)
