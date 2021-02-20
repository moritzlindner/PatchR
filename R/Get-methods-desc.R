#' Accession methods
#'
#' These methods are used to access information from \linkS4class{PRecording} and/or \linkS4class{PCollection} objects
#'
#' @param X A \linkS4class{PRecording} or \linkS4class{PCollection} object
#' @param which A name of a valid Group in a \linkS4class{PCollection} (for \code{GetGroupMembers}). \cr A name or a vector of names of slot(s) in RecordingParams (for \code{GetRecParam}). \cr A name or a vector of names of a column in the MetaData slot (for \code{GetMetaData}).
#' @details These methods can be used to access information stored in \linkS4class{PRecording} and/or \linkS4class{PCollection} objects. \cr \cr
#' @return A numeric vector.
#' @examples
#' GetRecParam(recording,c("Cs","Rs"))
#' @name Get-methods
NULL
