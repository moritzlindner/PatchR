#' Add data (Trace) to PRecording objects
#'
#' This function adds a new Trace (with data) from any object convertible into a matrix to a \linkS4class{PRecording} object.
#'
#' @param object A \linkS4class{PRecording} object
#' @param Trace Name of the new trace
#' @param Unit The SI unit of the trace
#' @param Sweeps Names of the sweeps added. Must be the same as sweep names in \var{object}. Data will be sorted accoding to order of Sweeps in \var{object} Default is \code{colnames(object)}
#' @param mtx Any object convertible into a \var{matrix}, that has the same dimension as data in the Data slot of \var{object}
#' @param isOrig if TRUE, marks added trace as an original recording.
#' @seealso \linkS4class{PRecording}, \linkS4class{PCollection}, \link[=base:as.matrix]{as.matrix()}
#' @return A matrix or \linkS4class{PRecording} object
#' @exportMethod AddTrace
setGeneric(name="AddTrace",
           def=function(object,
                        Trace,
                        Unit,
                        Sweeps=colnames(mtx),
                        isOrig=F,
                        mtx)
           {
             standardGeneric("AddTrace")
           }
)
setMethod("AddTrace",
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

            if(all(GetSweepNames(object) %in% Sweeps) && length(GetSweepNames(object))== length(Sweeps)){
              if(!is.ordered(Sweeps)){
                Sweeps<-ordered(Sweeps,levels=GetSweepNames(object))
              }
            }else{
              print("Sweep definitions do not match.")
            }

            if(!(Trace %in% object@Traces)){
              if(dim(object@Data[[1]])[1] == dim(mtx)[1] && GetSweepNames(object) == Sweeps){
                object@Data[[Trace]]<-as.matrix(mtx)
                object@Traces<-c(GetTraceNames(object),Trace)
                object@Units<-c(object@Units,Unit)
                if(isOrig){
                  object@RecordingParams@Traces<-c(GetTraceNames(object@RecordingParams),Trace)
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
