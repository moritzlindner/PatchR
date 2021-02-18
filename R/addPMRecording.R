

#' Adds PRecording to PCollection
#'
#' Adds a \linkS4class{PRecording} to existing \linkS4class{PCollection}
#'
#' @param object a \linkS4class{PRecording} object
#' @param PRecording a PRecording object, or list of PRecording objects
#' @param Names names for imported PRecording. Standard is variable name given in PRecording if single PRecording, i
#' @param Group Group variable or list thereof, of the same lenght as PRecording
#' @return A \linkS4class{PCollection} object
#' @exportMethod addPRecording
setGeneric(name="addPRecording",
           def=function(object,
                        PRecording,
                        Names=if(is.list(PRecording)){
                          lapply(PRecording,function(x) x@RecordingParams@Filename)
                        }else{
                          PRecording@RecordingParams@Filename
                        },
                        Group)
           {
             standardGeneric("addPRecording")
           }
)
setMethod("addPRecording",
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
