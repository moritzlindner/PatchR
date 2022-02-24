
#' Drops recording(s) from PCollection
#'
#' `r lifecycle::badge("stable")` \cr
#' Drops recording(s) from PCollection
#'
#' @param X A PCollection object
#' @param Recording A name of a \linkS4class{PRecording} in the collection, or a list thereof. If provided, drops that/those \linkS4class{PRecording}(s).
#' @param Group A name of a group in the\linkS4class{PCollection}. If provided, drops all \linkS4class{PRecording} belonging to that group. Either recording or group need to be given.
#' @return A \linkS4class{PCollection} object
#' @name DropPRecording
#' @examples
#' data("PRecording")
#' # first create a PCollection for testing
#' SampleCollection<-NewPCollection(SampleData,Names="A")
#' SampleCollection<-AddPRecording(SampleCollection, SampleData)
#' SampleCollection
#' # now drop the first Recording stored in it
#' SampleCollection<-DropPRecording(SampleCollection, GetRecordingNames(SampleCollection)[1])
#' SampleCollection
#' @exportMethod DropPRecording
setGeneric(name="DropPRecording",
           def=function(X,
                        Recording=NULL,
                        Group=NULL)
           {
             standardGeneric("DropPRecording")
           }
)
#' @noMd
setMethod("DropPRecording",
          "PCollection",
          function(X,
                   Recording=NULL,
                   Group=NULL)
          {
            warning("MetaData and Plots droped for consistency..")
            if (!is.null(Recording) & !is.null(Group)){
              stop("Only Group or Recording may be provided")
            }
            if(!is.null(Group)){
              Recording<-GetGroupMembers(X,Group)
            }
            Group<-X@Group[!(X@Names %in% Recording)]
            Group<-droplevels(Group)

            out<-PCollection(
              Recordings=X@Recordings[!(X@Names %in% Recording)],
              Names=X@Names[!(X@Names %in% Recording)],
              Group=Group,
              RecordingParams=X@RecordingParams
            )
            out
          }
)
