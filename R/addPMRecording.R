

#' Adds PRecording to PCollection
#'
#' Adds a \linkS4class{PRecording} to existing \linkS4class{PCollection}
#'
#' @param object A \linkS4class{PRecording} object
#' @param PRecording a \var{PRecording}  object, or \var{list} of \var{PRecording} objects.
#' @param Names Vames for imported a \var{PRecording}. Standard is File name stored in the added a \var{PRecording}(s).
#' @param Group Group variable or list thereof, of the same length as \var{PRecording}
#' @return A \linkS4class{PCollection} object
#' @exportMethod AddPRecording
setGeneric(name="AddPRecording",
           def=function(object,
                        PRecording,
                        Names=if(is.list(PRecording)){
                          lapply(PRecording,function(x) x@RecordingParams@Filename)
                        }else{
                          PRecording@RecordingParams@Filename
                        },
                        Group)
           {
             standardGeneric("AddPRecording")
           }
)
setMethod("AddPRecording",
          "PCollection",
          function(object,
                   PRecording,
                   Names=if(is.list(PRecording)){
                     lapply(PRecording,function(x) x@RecordingParams@Filename)
                     }else{
                       PRecording@RecordingParams@Filename
                       },
                   Group="Generic")
            {
            warning("MetaData and Plots are dropped to assure conistency.")

            if(length(Group)==1 & length(PRecording)>1){
              Group<-rep(Group,length(PRecording))
            }
            if(length(Group)!=length(PRecording)){
              stop("Incorrect length of 'Group'")
            }
            if(length(Names)!=length(PRecording)){
              stop("Incorrect length of 'Names'")
            }

            object@Series<-append(object@Series,PRecording)
            object@Names<-append(object@Names,Names)
            object@Group<-as.factor(c(as.character(object@Group),as.character(Group)))

            out<-PCollection(
              Series=object@Series,
              Names=object@Names,
              Group=object@Group,
              RecordingParams=object@RecordingParams
            )
            out
          }
          )
