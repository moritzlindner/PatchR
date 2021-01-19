setGeneric(name="ImportPMSeries",
           def=function(filename,
                        experiment=1,
                        series=1,
                        traces=c(1,2))
           {
             standardGeneric("ImportPMFile")
           }
)

#' Import Series from Patch Master *.dat file into a PMTrace object
#'
#' This function imports Series from Patch Master *.dat files and creates PMTrace objects
#'
#' @param filename path to a Patch Master *.dat file.
#' @param experiment Index of Experiment to import (1st order hirarchy in PM dat file).
#' @param series Index of Series to import (2nd order hirarchy in PM dat file).
#' @param traces Traces to import (indices). Must be vector of numerics of length > 0.
#' @exportMethod SubsetData
setMethod("ImportPMSeries",
          "PMTrace",
          function(filename,
                   experiment=1,
                   series=1,
                   traces=c(1,2)){
            print(paste("Importing", filename))
            first=T
            for(i in traces){
              print(paste("Importing trace", i))
              ephys2env<-getSeries(FN,file=1,exp=exp,ser=ser,trace=i)
              if(first){
                params<-PMRecordingParams(
                  Traces=epys2env$sweeps$tracename,
                  RecMode=ephys2env$sweeps$RecMode,
                  ProtocolName=epys2env$sweeps$Stimulus,
                  RPip=ephys2env$sweeps$RPip,
                  RSeal=ephys2env$sweeps$RSeal,
                  Urest=ephys2env$sweeps$Urest,
                  Cs=ephys2env$sweeps$Cs,
                  Rs=ephys2env$sweeps$Rs,
                  Experiment=epys2env$sweeps$exp,
                  Series=epys2env$sweeps$ser,
                  Created=Sys.time(), # FIXME this should better be the timestamp from the *.dat File
                  Filename=epys2env$sweeps$filename)

                out<-PMTrace(Traces=epys2env$sweeps$tracename,
                             Units=ephys2env$sweeps$YUnit,
                             TimeTraceepys2env$sweeps$x[,1],
                             Sweeps=ordered(colnames(epys2env$sweeps$y),levels=colnames(epys2env$sweeps$y)),
                             SweepTimes=as.vector(epys2env$sweeps$Trace_Time),
                             Data=list(epys2env$sweeps$tracename=as.matrix(epys2env$sweeps$y)),
                             Plots=list(),
                             RecordingParams=params
                             )
              }

            }else{
              out<-addTrace(object=out,
                              Trace=epys2env$sweeps$tracename,
                              Unit=ephys2env$sweeps$YUnit,
                              Data=epys2env$sweeps$y,
                              isOrig=T)
            }
            out
          }
)
