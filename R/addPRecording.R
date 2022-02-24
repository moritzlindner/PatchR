#' Adds PRecording to PCollection
#'
#' `r lifecycle::badge("stable")` \cr
#' Adds a \linkS4class{PRecording} to an existing \linkS4class{PCollection}
#'
#' @param X A \linkS4class{PRecording} object
#' @param PRecording a \var{PRecording}  object, or \var{list} of \var{PRecording} objects.
#' @param Names Name(s) for imported \var{PRecording}(s). Standard is the source file name as stored in the added a \var{PRecording}(s).
#' @param Group Group variable or list thereof, of the same length as \var{PRecording}
#' @return A \linkS4class{PCollection} object
#' @examples
#' # create a PCollection for testing
#' SampleCollection<-NewPCollection(SampleData,Names="A")
#' SampleCollection
#' # now add a PRecording
#' SampleCollection<-AddPRecording(SampleCollection, SampleData)
#' SampleCollection
#' @name AddPRecording
#' @exportMethod AddPRecording
setGeneric(
  name = "AddPRecording",
  def = function(X,
                 PRecording,
                 Names = if (is.list(PRecording)) {
                   lapply(PRecording, function(x)
                     x@RecordingParams@Filename)
                 } else{
                   PRecording@RecordingParams@Filename
                 },
                 Group = "Generic")
  {
    standardGeneric("AddPRecording")
  }
)

#' @noRd
setMethod("AddPRecording",
          "PCollection",
          function(X,
                   PRecording,
                   Names = if (is.list(PRecording)) {
                     lapply(PRecording, function(x)
                       x@RecordingParams@Filename)
                   } else{
                     PRecording@RecordingParams@Filename
                   },
                   Group = "Generic")
          {
            warning("MetaData and Plots droped for consistency.")

            if(is.list(Group)){
              if (is.list(PRecording)){
                if (length(Group) != length(PRecording)){
                  stop("Incorrect length of 'Group'")
                }
              }else{
                stop("Incorrect length of 'Group'")
              }
            }else{
              if (is.list(PRecording)){
                Group <- rep(Group, length(PRecording))
              }
            }
            if (is.list(Names)){
              if (!is.list(PRecording) || (length(Names) != length(PRecording))){
                stop("Incorrect length of 'Names'")
              }
            }else{
              if (is.list(PRecording)){
                stop("Incorrect length of 'Names'")
              }
            }

            X@Recordings <- append(X@Recordings, PRecording)
            X@Names <- append(X@Names, Names)
            X@Group <-
              as.factor(c(as.character(X@Group), as.character(Group)))

            out <- PCollection(
              Recordings = X@Recordings,
              Names = X@Names,
              Group = X@Group,
              RecordingParams = X@RecordingParams
            )
            out
          })
