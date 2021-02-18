setGeneric(name="dropPRecording",
           def=function(object,
                        PRecording=NULL,
                        Group=NULL)
           {
             standardGeneric("dropPRecording")
           }
)

#' Drops PRecording(s) from PCollection
#'
#'  Drops PRecording(s) from PCollection
#'
#' @param object a PCollection object
#' @param PRecording a name of a PRecording object in the collection, or a list thereof
#' @return A \link[=PCollection]{PCollection} object
#' @exportMethod dropPRecording
setMethod("dropPRecording",
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
              PRecording<-getGroupMembers(object,Group)
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
