setGeneric(name="MeasureSweeps",
           def=function(object,
                        Trace="I-mon",
                        Sweeps=getSweepNames(object),
                        Time,
                        label,
                        fun=mean,
                        ReturnPMCollection=T)
           {
             standardGeneric("MeasureSweeps")
           }
)
#' Perform measurements on all recordings in the collection on a per-sweep basis.
#'
#' This function performs measurements on all recordings in the collection for a given trace on a per-sweep basis. This can be useful e.g. for generating data underlying a Time Series Plot.
#'
#' @param object a \link[=PMCollection]{PMCollection} object
#' @param Trace Trace to perform Stats on
#' @param Sweeps List of Sweeps to process
#' @param Time either a range of time points to keep, or, if \code{fun} is a binary operator then two particular time points
#' @param label A label (if \code{Sweeps} has length 1) or a prefix
#' @param fun function to apply on sweep. Can be anything that woks with \link[=base::apply]{apply}. But will be usually \link[=base::mean]{mean}, \link[=base::max]{max}, \link[=base::min]{min}, or \link[=base::`-`]{`-`}.
#' @param ReturnPMCollection wheter to return a PMCollection or a Matrix.
#' @return A \link[=PMRecording]{PMRecording} object with an updated MetaData Slot or a matrix.
#' @exportMethod MeasureSweeps
setMethod("MeasureSweeps",
          "PMCollection",
          function(object,
                   Trace="I-mon",
                   Sweeps=getSweepNames(object),
                   Time,
                   label,
                   fun=mean,
                   ReturnPMCollection=T
                   ){
            if(length(Trace)>1){
              stop("This function can only be applied to a single Trace")
              }
            if(length(Sweeps)>1){
              label<-paste0(label,".",Sweeps)
            }
            if(as.character(substitute(fun)) %in% c("+","-","+","/","^","**")){
              message(paste("Binary operator", as.character(substitute(fun)), "applied. Use Time vector as exclusive points."))
              TimeExclusive=T
              }else{
                TimeExclusive=F
                }
            out<-lapply(object,
                        function(x){
                          apply(SubsetData(x,
                                           Traces=Trace,
                                           Time =Time,
                                           Sweeps=Sweeps,
                                           nowarnings = T,
                                           TimeExclusive=TimeExclusive),
                                "Time",
                                FUN = fun)
                          }
                        )
            if (!ReturnPMCollection){
              out<-as.matrix(out)
              colnames(out)<-label
              rownames(out)<-object@Names
              return(out)
              }else{
                return(AddMetaData(object,
                                   out,
                                   title=label)
                )
              }
            }
          )
