#' Import Series from Patch Master *.dat file into a PMRecording object
#'
#' This function imports Series from Patch Master *.dat files and creates PMRecording objects
#'
#' @param filename path to a [Patch Master](https://www.heka.com/downloads/downloads_main.html#down_patchmaster) *.dat file.
#' @param experiment Index of Experiment to import (1st order hirarchy in PM dat file).
#' @param series Index of Series to import (2nd order hirarchy in PM dat file).
#' @param traces Traces to import (indices). Must be vector of numerics of length > 0.
#' @return A \link[=PMRecording]{PMRecording} object
#' @export
ImportPMRecording<-function(filename,
                   experiment=1,
                   series=1,
                   traces=c(1,2)){
  print(paste("Importing", filename))
  first=T
  for(i in traces){
    print(paste("Importing trace", i))
    imp<-getSeries(filename,file=1,exp=experiment,ser=series,trace=i)
    if(first){
      first=F
      params<-PMRecordingParams(
        Traces=imp$sweeps$tracename,
        RecMode=imp$sweeps$RecMode,
        ProtocolName=imp$sweeps$Stimulus,
        RPip=imp$sweeps$RPip,
        RSeal=imp$sweeps$RSeal,
        Urest=imp$sweeps$Urest,
        Cs=imp$sweeps$Cs,
        Rs=imp$sweeps$Rs,
        Experiment=imp$sweeps$exp,
        Series=imp$sweeps$ser,
        Created=Sys.time(), # FIXME this should better be the timestamp from the *.dat File
        Filename=imp$sweeps$filename)

      Data<-list()
      Data[[imp$sweeps$tracename]]<-as.matrix(imp$sweeps$y)

      out<-PMRecording(Traces=imp$sweeps$tracename,
                   Units=imp$sweeps$YUnit,
                   getTimeTrace=imp$sweeps$x[,1],
                   TimeUnit = imp$sweeps$XUnit,
                   Sweeps=ordered(colnames(imp$sweeps$y),levels=colnames(imp$sweeps$y)),
                   SweepTimes=as.numeric(as.vector(imp$sweeps$Trace_Time)),
                   Data=Data,
                   Plots=list(),
                   RecordingParams=params
                   )
    }else{
    out<-addTrace(object=out,
                    Trace=imp$sweeps$tracename,
                    Unit=imp$sweeps$YUnit,
                    mtx=imp$sweeps$y,
                    isOrig=T)
  }
  }
  return(out)
}
