#' Add data (Trace) to PRecording objects
#'
#' This function adds a new Trace (with data) from any object convertible into a matrix to a PRecording object
#'
#' @param object A \linkS4class{PRecording} object
#' @param Trace Name of the new Trace
#' @param Sweeps Names of the sweeps added. Must be the same as sweep names in \code{object}. Data will be sorted accoding to order of Sweeps in \code{object} Default is \code{colnames(object)}
#' @param mtx Any object convertible into a matrix, that has the same dimension as data in the Data slot of \code{object}
#' @return A matrix or \linkS4class{PRecording} object
#' @exportMethod addTrace
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
setMethod("addTrace",
          "PRecording",
          function(object,
                   Trace,
                   Unit,
                   Sweeps=colnames(mtx),
                   isOrig=F,
                   mtx)
          {
            if(!PatchR:::validPRecording(object)){
              stop(paste(deparse(substitute(object)), "is not a valid PRecording"))
            }

            if(all(getSweepNames(object) %in% Sweeps) && length(getSweepNames(object))== length(Sweeps)){
              if(!is.ordered(Sweeps)){
                Sweeps<-ordered(Sweeps,levels=getSweepNames(object))
              }
            }else{
              print("Sweep definitions do not match.")
            }

            if(!(Trace %in% object@Traces)){
              if(dim(object@Data[[1]])[1] == dim(mtx)[1] && getSweepNames(object) == Sweeps){
                object@Data[[Trace]]<-as.matrix(mtx)
                object@Traces<-c(getTraceNames(object),Trace)
                object@Units<-c(object@Units,Unit)
                if(isOrig){
                  object@RecordingParams@Traces<-c(getTraceNames(object@RecordingParams),Trace)
                }
              }else{
                stop("Data dimension mismatch")
              }
            }else{
              stop(paste("Trace",Trace,"already in",deparse(substitute(object))))
            }
            if(!PatchR:::validPRecording(object)){
              stop(paste("updating PRecording", deparse(substitute(object)), "failed."))
            }
            object
          }
)
