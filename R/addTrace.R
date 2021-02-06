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

#' Add data (Trace) to PMSeries objects
#'
#' This function adds a new Trace (with data) from any object convertible into a matrix to a PMSeries object
#'
#' @param object A PMSeries object
#' @param Trace Name of the new Trace
#' @param Sweeps Names of the sweeps added. Must be the same as sweep names in \code{object}. Data will be sorted accoding to order of Sweeps in \code{object} Default is \code{colnames(object)}
#' @param mtx Any object convertible into a matrix, that has the same dimension as data in the Data slot of \code{object}
#' @return A matrix or \link[=PMSeries]{PMSeries} object
#' @exportMethod addTrace
setMethod("addTrace",
          "PMSeries",
          function(object,
                   Trace,
                   Unit,
                   Sweeps=colnames(mtx),
                   isOrig=F,
                   mtx)
          {
            if(!PatchMasteR:::validPMSeries(object)){
              stop(paste(deparse(substitute(object)), "is not a valid PMSeries"))
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
                object@Units<-c(object@Units,Unit)
                if(isOrig){
                  object@RecordingParams@Traces<-c(getTraces(object@RecordingParams),Trace)
                }
              }else{
                stop("Data dimension mismatch")
              }
            }else{
              stop(paste("Trace",Trace,"already in",deparse(substitute(object))))
            }
            if(!PatchMasteR:::validPMSeries(object)){
              stop(paste("updating PMSeries", deparse(substitute(object)), "failed."))
            }
            object
          }
)
