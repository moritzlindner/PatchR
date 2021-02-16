setGeneric(name="MeasureSweeps",
           def=function(object,
                        Trace="I-mon",
                        Sweeps=SweepNames(object),
                        Time,
                        label,
                        fun=mean)
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
#' @param fun function to apply on sweep. Can be anything that woks with \link[=base::apply]{apply}. But will be usuall \link[=base::mean]{mean}, \link[=base::max]{max}, \link[=base::min]{min}, or \link[=base::`-`]{`-`}.
#' @return A \link[=PMRecording]{PMRecording} object with an updated MetaData Slot.
#' @exportMethod MeasureSweeps
setMethod("MeasureSweeps",
          "PMCollection",
          function(object,
                   Trace="I-mon",
                   Sweeps=SweepNames(object),
                   Time,
                   label,
                   fun=mean
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
            AddMetaData(object,
                        lapply(object,
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
                               ),
                        title=label
                        )
          }
)
