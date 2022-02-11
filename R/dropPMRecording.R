
#' Drops PRecording(s) from PCollection
#'
#'  Drops PRecording(s) from PCollection
#'
#' @param object a PCollection object
#' @param PRecording A name of a \linkS4class{PRecording} object in the collection, or a list thereof
#' @param Group A name of A group in the\linkS4class{PRecording} object Drops all \linkS4class{PRecording} belonging to that group. Alternative to \var{PRecording}.
#' @return A \linkS4class{PCollection} object
#' @name DropPRecording
#' @exportMethod DropPRecording
setGeneric(name="DropPRecording",
           def=function(object,
                        PRecording=NULL,
                        Group=NULL)
           {
             standardGeneric("DropPRecording")
           }
)
#' @describeIn DropPRecording Method for PCollection
setMethod("DropPRecording",
          "PCollection",
          function(object,
                   PRecording=NULL,
                   Group=NULL)
          {
            warning("MetaData and Plots are dropped to assure conistency.")
            if (!is.null(PRecording) & !is.null(Group)){
              stop("Only Group or PRecording may be provided")
            }
            if(!is.null(Group)){
              PRecording<-GetGroupMembers(object,Group)
            }
            Group<-object@Group[!(object@Names %in% PRecording)]
            Group<-droplevels(Group)

            out<-PCollection(
              Series=object@Series[!(object@Names %in% PRecording)],
              Names=object@Names[!(object@Names %in% PRecording)],
              Group=Group,
              RecordingParams=object@RecordingParams
            )
            out
          }
)
