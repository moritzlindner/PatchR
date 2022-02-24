#' Add a trace/channel to PRecording objects
#'
#' `r lifecycle::badge("stable")` \cr
#' This function adds a new trace /channel from any object convertible into a matrix to an existing \linkS4class{PRecording} X.
#'
#' @param X A \linkS4class{PRecording} object
#' @param mtx Any object convertible into a \var{matrix}. Rows are sampling points, columns are sweeps. Number of sampling points and sweeps must match the traces already present in \var{X}
#' @param Trace Name of the new trace
#' @param Unit The unit of the trace
#' @param Sweeps Names of the sweeps added. Must match the sweep names in \var{X}. Data in \var{Trace} will be sorted according to the order of sweeps in \var{X} before adding. Default is \code{colnames(X)}
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
