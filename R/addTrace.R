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

#' Add data (Trace) to PMRecording objects
#'
#' This function adds a new Trace (with data) from any object convertible into a matrix to a PMRecording object
#'
#' @param object A PMRecording object
#' @param Trace Name of the new Trace
#' @param Sweeps Names of the sweeps added. Must be the same as sweep names in \code{object}. Data will be sorted accoding to order of Sweeps in \code{object} Default is \code{colnames(object)}
#' @param mtx Any object convertible into a matrix, that has the same dimension as data in the Data slot of \code{object}
#' @return A matrix or \link[=PMRecording]{PMRecording} object
#' @exportMethod addTrace
setMethod("addTrace",
          "PMRecording",
          function(object,
                   Trace,
                   Unit,
                   Sweeps=colnames(mtx),
                   isOrig=F,
                   mtx)
          {
            if(!PatchMasteR:::validPMRecording(object)){
              stop(paste(deparse(substitute(object)), "is not a valid PMRecording"))
            }

            if(all(SweepNames(object) %in% Sweeps) && length(SweepNames(object))== length(Sweeps)){
              if(!is.ordered(Sweeps)){
                Sweeps<-ordered(Sweeps,levels=SweepNames(object))
              }
            }else{
              print("Sweep definitions do not match.")
            }

            if(!(Trace %in% object@Traces)){
              if(dim(object@Data[[1]])[1] == dim(mtx)[1] && SweepNames(object) == Sweeps){
                object@Data[[Trace]]<-as.matrix(mtx)
                object@Traces<-c(TraceNames(object),Trace)
                object@Units<-c(object@Units,Unit)
                if(isOrig){
                  object@RecordingParams@Traces<-c(TraceNames(object@RecordingParams),Trace)
                }
              }else{
                stop("Data dimension mismatch")
              }
            }else{
              stop(paste("Trace",Trace,"already in",deparse(substitute(object))))
            }
            if(!PatchMasteR:::validPMRecording(object)){
              stop(paste("updating PMRecording", deparse(substitute(object)), "failed."))
            }
            object
          }
)
