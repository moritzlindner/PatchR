setGeneric(name="CurrentDensity",
           def=function(object,
                        trace="I.mon")
           {
             standardGeneric("CurrentDensity")
           }
)

#' CurrentDensity
#'
#' This function normalizes current traces to cell capacity
#'
#' @name CurrentDensity
#' @param object a PMTrace object
#' @import sitools
#' @exportMethod CurrentDensity
setMethod("CurrentDensity",
          "PMTrace",
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
