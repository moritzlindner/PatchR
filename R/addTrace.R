#' (OK) Add data (Trace) to PRecording objects
#'
#' `r lifecycle::badge("stable")` \cr
#' This function adds a new Trace (with data) from any object convertible into a matrix to a \linkS4class{PRecording} X.
#'
#' @param X A \linkS4class{PRecording} object
#' @param Trace Name of the new trace
#' @param Unit The SI unit of the trace
#' @param Sweeps Names of the sweeps added. Must be the same as sweep names in \var{X}. Data will be sorted accoding to order of Sweeps in \var{X} Default is \code{colnames(X)}
#' @param mtx Any X convertible into a \var{matrix}, that has the same dimension as data in the Data slot of \var{X}
#' @param isOrig if TRUE, marks added trace as an original recording.
#' @seealso \linkS4class{PRecording}, \linkS4class{PCollection}, \link[base:as.matrix]{as.matrix()}
#' @return A matrix or \linkS4class{PRecording} X
#' @name AddTrace
#' @exportMethod AddTrace
setGeneric(name="AddTrace",
           def=function(X,
                        mtx,
                        Trace,
                        Unit,
                        Sweeps=colnames(mtx),
                        isOrig=F)
           {
             standardGeneric("AddTrace")
           }
)

#' @noRd
setMethod("AddTrace",
          "PRecording",
          function(X,
                   mtx,
                   Trace,
                   Unit,
                   Sweeps=colnames(mtx),
                   isOrig=F)
          {
            if(!validPRecording(X)){
              stop(paste(deparse(substitute(X)), "is not a valid PRecording"))
            }

            if(all(GetSweepNames(X) %in% Sweeps) && length(GetSweepNames(X))== length(Sweeps)){
              if(!is.ordered(Sweeps)){
                Sweeps<-ordered(Sweeps,levels=GetSweepNames(X))
              }
            }else{
              message("Sweep definitions do not match.")
            }

            if(!(Trace %in% X@Traces)){
              if(dim(X@Data[[1]])[1] == dim(mtx)[1] && GetSweepNames(X) == Sweeps){
                X@Data[[Trace]]<-as.matrix(mtx)
                X@Traces<-c(GetTraceNames(X),Trace)
                X@Units<-c(X@Units,Unit)
                if(isOrig){
                  X@RecordingParams@Traces<-c(GetTraceNames(X@RecordingParams),Trace)
                }
              }else{
                stop("Data dimension mismatch")
              }
            }else{
              stop(paste("Trace",Trace,"already in",deparse(substitute(X))))
            }
            if(!validPRecording(X)){
              stop(paste("updating PRecording", deparse(substitute(X)), "failed."))
            }
            X
          }
)
