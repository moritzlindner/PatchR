setGeneric(name="addPMCollection",
           def=function(object,
                        PMRecording,
                        Names=if(is.list(PMRecording)){
                          lapply(PMRecording,function(x) x@RecordingParams@Filename)
                        }else{
                          PMRecording@RecordingParams@Filename
                        },
                        Group)
           {
             standardGeneric("addPMCollection")
           }
)

#' Adds PMRecording to PMCollection
#'
#' Adds PMRecording to existing PMCollection
#'
#' @param object a PMCollection object
#' @param PMRecording a PMRecording object, or list of PMRecording objects
#' @param Names names for imported PMRecording. Standard is variable name given in PMRecording if single PMRecording, i
#' @param Group Group variable or list thereof, of the same lenght as PMRecording
#' @return A \link[=PMCollection]{PMCollection} object
#' @exportMethod addPMCollection
setMethod("addPMCollection",
          "PMCollection",
          function(object,
                   PMRecording,
                   Names=if(is.list(PMRecording)){
                     lapply(PMRecording,function(x) x@RecordingParams@Filename)
                     }else{
                       PMRecording@RecordingParams@Filename
                       },
                   Group="Generic")
            {
            warning("MetaData and Plots are dropped to assure conistency.")

            if(length(Group)==1 & length(PMRecording)>1){
              Group<-rep(Group,length(PMRecording))
            }
            if(length(Group)!=length(PMRecording)){
              stop("Incorrect length of 'Group'")
            }
            if(length(Names)!=length(PMRecording)){
              stop("Incorrect length of 'Names'")
            }

            object@Series<-append(object@Series,PMRecording)
            object@Names<-append(object@Names,Names)
            object@Group<-as.factor(c(as.character(object@Group),as.character(Group)))

            out<-PMCollection(
              Series=object@Series,
              Names=object@Names,
              Group=object@Group,
              RecordingParams=object@RecordingParams
            )
            out
          }
          )
