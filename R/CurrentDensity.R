#' Normalizes current traces to cell capacity
#'
#' This function normalizes current traces to cell capacity and adds it as a new Trace
#'
#' @inheritParams Get
#' @param Trace Name of trace containing current data to normalize to cell capacity.
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

#' @describeIn CurrentDensity Method for PRecording
setMethod("CurrentDensity",
          "PRecording",
          function(X,
                   Trace="I.mon"){
            print(paste0("Capacitance in record: ",f2si(round(GetCSlow(X),15)),"F"))

            X<-AddTrace(X,
              Trace="curr.dens",
              Unit=paste0(X@Units[GetTraceNames(X)==Trace],"/F"),
              Sweeps=GetSweepNames(X),
              mtx=objcet@Data[[Trace]]/GetCSlow(X))

            X
            }
          )

#' @describeIn CurrentDensity Method for PCollection
setMethod("CurrentDensity",
          "PCollection",
          function(X,
                   Trace="I.mon"){
            lapply(X,function(y){CurrentDensity(y,Trace)},ReturnPMObject=T)
          }
)
