setGeneric(name="dropPMRecording",
           def=function(object,
                        PMRecording=NULL,
                        Group=NULL)
           {
             standardGeneric("dropPMRecording")
           }
)

#' Drops PMRecording(s) from PMCollection
#'
#'  Drops PMRecording(s) from PMCollection
#'
#' @param object a PMCollection object
#' @param PMRecording a name of a PMRecording object in the collection, or a list thereof
#' @return A \link[=PMCollection]{PMCollection} object
#' @exportMethod dropPMRecording
setMethod("dropPMRecording",
          "PMCollection",
          function(object,
                   PMRecording=NULL,
                   Group=NULL)
          {
            warning("MetaData and Plots are dropped to assure conistency.")
            if (!is.null(PMRecording) & !is.null(Group)){
              stop("Only Group or PMRecording may be provided")
            }
            if(!is.null(Group)){
              PMRecording<-getGroupMembers(object,Group)
            }
            Group<-object@Group[!(object@Names %in% PMRecording)]
            Group<-droplevels(Group)

            out<-PMCollection(
              Series=object@Series[!(object@Names %in% PMRecording)],
              Names=object@Names[!(object@Names %in% PMRecording)],
              Group=Group,
              RecordingParams=object@RecordingParams
            )
            out
          }
)
