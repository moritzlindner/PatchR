
#' (OK) Drops Recording(s) from PCollection
#'
#' `r lifecycle::badge("stable")` \cr
#' Drops Recording(s) from PCollection
#'
#' @param X a PCollection X
#' @param Recording A name of a \linkS4class{PRecording} in the collection, or a list thereof
#' @param Group A name of A group in the\linkS4class{PCollection}. Drops all \linkS4class{PRecording} belonging to that group. Either Recording or Group need to be given
#' @return A \linkS4class{PCollection} X
#' @name DropPRecording
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
            warning("MetaData and Plots are dropped to assure conistency.")
            if (!is.null(Recording) & !is.null(Group)){
              stop("Only Group or Recording may be provided")
            }
            if(!is.null(Group)){
              Recording<-GetGroupMembers(X,Group)
            }
            Group<-X@Group[!(X@Names %in% Recording)]
            Group<-droplevels(Group)

            out<-PCollection(
              Series=X@Series[!(X@Names %in% Recording)],
              Names=X@Names[!(X@Names %in% Recording)],
              Group=Group,
              RecordingParams=X@RecordingParams
            )
            out
          }
)
