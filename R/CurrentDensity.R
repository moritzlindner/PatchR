setGeneric(name="CurrentDensity",
           def=function(object,
                        trace="I.mon")
           {
             standardGeneric("CurrentDensity")
           }
)

#' Normalizes current traces to cell capacity
#'
#' This function normalizes current traces to cell capacity and adds it as a new Trace
#'
#' @param object a \link[=PRecording]{PRecording} object
#' @return a \link[=PRecording]{PRecording} object
#' @importFrom  sitools f2si
#' @exportMethod CurrentDensity
setMethod("CurrentDensity",
          "PRecording",
          function(object,
                   trace="I.mon"){
            print(paste0("Capacitance in record: ",f2si(round(GetCSlow(object),15)),"F"))

            object<-AddTrace(object,
              Trace="curr.dens",
              Unit=paste0(object@Units[getChannels(object)==trace],"/F"),
              Sweeps=GetSweepNames(object),
              mtx=objcet@Data[[trace]]/GetCSlow(object))

            object

            }
          )
