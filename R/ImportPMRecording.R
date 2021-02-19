#' Import Series from Patch Master *.dat file into a PRecording object
#'
#' This function imports Series from Patch Master *.dat files and creates PRecording objects
#'
#' @param filename path to a [Patch Master](https://www.heka.com/downloads/downloads_main.html#down_patchmaster) *.dat file.
#' @param experiment Index of Experiment to import (1st order hierarchy in PM dat file).
#' @param series Index of Series to import (2nd order hierarchy in PM dat file).
#' @param traces Traces to import (indices). Must be vector of numeric of length > 0.
#' @param encoding file encoding to use, default is \code{getOption("encoding")}
#' @return A \link[=PRecording]{PRecording} object
#' @export
ImportPRecording<-function(filename,
                   experiment=1,
                   series=1,
                   traces=c(1,2),
                   encoding=getOption("encoding")){
  print(paste("Importing", filename))
  first=T
  for(i in traces){
    print(paste("Importing trace", i))
    imp<-getSeries(filename,file=1,exp=experiment,ser=series,trace=i,encoding=encoding)
    if(first){
      first=F
      params<-PRecordingParams(
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
        Created=POSIXct(imp$sweeps$time),
        Filename=imp$sweeps$filename)

      Data<-list()
      Data[[imp$sweeps$tracename]]<-as.matrix(imp$sweeps$y)

      out<-PRecording(Traces=imp$sweeps$tracename,
                   Units=imp$sweeps$YUnit,
                   TimeTrace=imp$sweeps$x[,1],
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
