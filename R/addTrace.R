setGeneric(name="addTrace",
           def=function(object,
                        Trace,
                        Unit,
                        Sweeps=colnames(mtx),
                        isOrig=F,
                        mtx)
           {
             standardGeneric("addTrace")
           }
)

#' Adding data to PMTrace objects
#'
#' This function adds a new Trace (with data) from any object convertible into a matrix to the Data slot of PMTrace objects
#'
#' @param object A PMTrace object
#' @param Trace Name of the new Trace
#' @param Sweeps Names of the sweeps added. Default is colnames(mtx)
#' @param mtx Any object convertible into a matrix, that has the same dimension as data in the Data slot of object
#' @exportMethod SubsetData
setMethod("addTrace",
          "PMTrace",
          function(object,
                   Trace,
                   Unit,
                   Sweeps=colnames(mtx),
                   isOrig=F,
                   mtx)
          {
            if(!PatchMasteR:::validPMTrace(object)){
              stop(paste(deparse(substitute(object)), "is not a valid PMTrace"))
            }

            if(all(getSweeps(object) %in% Sweeps) && length(getSweeps(object))== length(Sweeps)){
              if(!is.ordered(Sweeps)){
                Sweeps<-ordered(Sweeps,levels=getSweeps(object))
              }
            }else{
              print("Sweep definitions do not match.")
            }

            if(!(Trace %in% object@Traces)){
              if(dim(object@Data[[1]])[1] == dim(mtx)[1] && getSweeps(object) == Sweeps){
                object@Data[[Trace]]<-as.matrix(mtx)
                object@Traces<-c(getTraces(object),Trace)
                object@Units<-c(object@Units,Units)
                if(isOrig){
                  object@RecordingParams@Traces<-c(getTraces(object@RecordingParams),Trace)
                }
              }else{
                stop("Data dimension mismatch")
              }
            }else{
              stop(paste("Trace",Trace,"already in",deparse(substitute(object))))
            }


            if(!PatchMasteR:::validPMTrace(object)){
              stop(paste("updating PMTrace", deparse(substitute(object)), "failed."))
            }
            object
          }
)
