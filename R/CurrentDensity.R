#' Normalizes current traces to cell capacity
#'
#' `r lifecycle::badge("stable")` \cr
#' This function normalizes current traces/channels to cell capacity and adds it as a new Trace
#'
#' @inheritParams Get
#' @param Trace Name of trace/channel containing current data to normalize to cell capacity.
#' @return A \linkS4class{PRecording} or \linkS4class{PCollection} object.
#' @name CurrentDensity
#' @exportMethod CurrentDensity
setGeneric(name="CurrentDensity",
def=function(X,
             Trace="I.mon")
{
  standardGeneric("CurrentDensity")
}
)

#' @noRd
setMethod("CurrentDensity",
          "PRecording",
          function(X,
                   Trace="I.mon"){
            if("curr.dens" %in% GetTraceNames(X)){
              stop("Current Density already computed.")
            }

            X<-AddTrace(X,
                        mtx=X@Data[[Trace]]/GetCSlow(X),
              Trace="curr.dens",
              Unit=paste0(X@Units[GetTraceNames(X)==Trace],"/F"),
              Sweeps=GetSweepNames(X))

            X
            }
          )

#' @noRd
setMethod("CurrentDensity",
          "PCollection",
          function(X,
                   Trace="I.mon"){
            lapply(X,function(y){CurrentDensity(y,Trace)},ReturnPObject=T)
          }
)
