

#' Adds PRecording to PCollection
#'
#' Adds a \linkS4class{PRecording} to existing \linkS4class{PCollection}
#'
#' @param X A \linkS4class{PRecording} object
#' @param PRecording a \var{PRecording}  object, or \var{list} of \var{PRecording} objects.
#' @param Names Vames for imported a \var{PRecording}. Standard is File name stored in the added a \var{PRecording}(s).
#' @param Group Group variable or list thereof, of the same length as \var{PRecording}
#' @return A \linkS4class{PCollection} object
#' @name AddPRecording
#' @exportMethod AddPRecording
setGeneric(name="AddPRecording",
           def=function(X,
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

#' @describeIn AddPRecording Method for PCollection
setMethod("AddPRecording",
          "PCollection",
          function(X,
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

            X@Series<-append(X@Series,PRecording)
            X@Names<-append(X@Names,Names)
            X@Group<-as.factor(c(as.character(X@Group),as.character(Group)))

            out<-PCollection(
              Series=X@Series,
              Names=X@Names,
              Group=X@Group,
              RecordingParams=X@RecordingParams
            )
            out
          }
          )
