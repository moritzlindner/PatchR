setGeneric(name="addPMExperiment",
           def=function(object,
                        PMSeries,
                        Names=if(is.list(PMSeries)){
                          lapply(PMSeries,function(x) x@RecordingParams@Filename)
                        }else{
                          PMSeries@RecordingParams@Filename
                        },
                        Group)
           {
             standardGeneric("addPMExperiment")
           }
)

#' Adds PMSeries to PMExperiment
#'
#' Adds PMSeries to existing PMExperiment
#'
#' @param object a PMExperiment object
#' @param PMSeries a PMSeries object, or list of PMSeries objects
#' @param Names names for imported PMSeries. Standard is variable name given in PMSeries if single PMSeries, i
#' @param Group Group variable or list thereof, of the same lenght as PMSeries
#' @return A \link[=PMExperiment]{PMExperiment} object
#' @exportMethod addPMExperiment
setMethod("addPMExperiment",
          "PMExperiment",
          function(object,
                   PMSeries,
                   Names=if(is.list(PMSeries)){
                     lapply(PMSeries,function(x) x@RecordingParams@Filename)
                     }else{
                       PMSeries@RecordingParams@Filename
                       },
                   Group="Generic")
            {
            warning("MetaData and Plots are dropped to assure conistency.")

            if(length(Group)==1 & length(PMSeries)>1){
              Group<-rep(Group,length(PMSeries))
            }
            if(length(Group)!=length(PMSeries)){
              stop("Incorrect length of 'Group'")
            }
            if(length(Names)!=length(PMSeries)){
              stop("Incorrect length of 'Names'")
            }

            object@Series<-append(object@Series,PMSeries)
            object@Names<-append(object@Names,Names)
            object@Group<-as.factor(c(as.character(object@Group),as.character(Group)))

            out<-PMExperiment(
              Series=object@Series,
              Names=object@Names,
              Group=object@Group,
              RecordingParams=object@RecordingParams
            )
            out
          }
          )
