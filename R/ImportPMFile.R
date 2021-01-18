setGeneric(name="ImportPMFile",
           def=function(filename,
                        experiment=1,
                        series=1,
                        traces=c(1,2))
           {
             standardGeneric("ImportPMFile")
           }
)

setMethod("ImportPMFile",
          "PMTrace",
          function(filename,
                   experiment=1,
                   series=1,
                   traces=c(1,2)){
            print(paste("Importing", filename))
            first=T
            for(i in traces){
              print(paste("Importing trace", i))
              Data<-list()
              ephys2env<-getSeries(FN,file=1,exp=exp,ser=ser,trace=i)
              if(first){
                params<-PMRecordingParams(
                  Channels=epys2env$sweeps$tracename,
                  RecMode=ephys2env$sweeps$RecMode,
                  ProtocolName=epys2env$sweeps$Stimulus,
                  RPip=ephys2env$sweeps$RPip,
                  RSeal=ephys2env$sweeps$RSeal,
                  Urest=ephys2env$sweeps$Urest,
                  Cs=ephys2env$sweeps$Cs,
                  Rs=ephys2env$sweeps$Rs,
                  Experiment=epys2env$sweeps$exp,
                  Series=epys2env$sweeps$ser,
                  Created=Sys.time(),
                  Filename=epys2env$sweeps$filename)

                Data[[epys2env$sweeps$tracename]]<-as.matrix(epys2env$sweeps$y)

                out<-PMTrace(Channels=epys2env$sweeps$tracename,
                             Units=ephys2env$sweeps$YUnit,
                             TimeTraceepys2env$sweeps$x[,1],
                             Sweeps=ordered(colnames(epys2env$sweeps$y),levels=colnames(epys2env$sweeps$y)),
                             SweepTimes=as.vector(epys2env$sweeps$Trace_Time),
                             Data=Data,
                             Plots=list(),
                             RecordingParams=params
                             )
              }

            }else{
              out<-addChannel(out,
                              Channel,
                              Unit,
                              Data,
                              isOrig) This function needs to be defined
            }
          }
)
