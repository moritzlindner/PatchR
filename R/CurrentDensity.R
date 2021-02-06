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
#' @param object a \link[=PMSeries]{PMSeries} object
#' @return a \link[=PMSeries]{PMSeries} object
#' @import sitools
#' @exportMethod CurrentDensity
setMethod("CurrentDensity",
          "PMSeries",
          function(object,
                   trace="I.mon"){
            print(paste0("Capacitance in record: ",sitools::f2si(round(getCs(object),15)),"F"))

            object<-addTrace(object,
              Trace="curr.dens",
              Unit=paste0(object@Units[getChannels(object)==trace],"/F"),
              Sweeps=getSweeps(object),
              mtx=objcet@Data[[trace]]/getCs(object))

            object

            }
          )
