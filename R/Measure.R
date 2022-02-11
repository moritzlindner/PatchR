#' Measurement methods
#'
#' These methods perform measurements on \linkS4class{PRecording} and \linkS4class{PCollection} objects on a per-sweep basis. They are designed to efficiently collect data e.g. for time series, dose-response or point statistics
#'
#' @inheritParams GetData
#' @param Trace The name of the \code{Trace} to perform measurements on
#' @param StimTrace The name of the \code{Trace} that contains the Stimulus
#' @param RespTrace The name of the \code{Trace} that contains the Response
#' @param label A label (if \code{Sweeps} has length 1) or a prefix
#' @param FUN function to apply on sweep. Can be anything that woks with \link[base:apply]{apply()}. But will be usually be \link[base:mean]{mean()}, \link[base:max]{max()}, \link[base:min]{min()}, or \link[base:Arithmetic]{`-`}.
#' @param ReturnPMObject whether to return a \linkS4class{PRecording}/\linkS4class{PCollection} or a matrix.
#' @name Measure
NULL

#' @describeIn Measure This function performs the actual measurements. It subsets an object by \code{Trace} and \code{Sweeps}. If \code{FUN} is a binary operator, then applies it to the exact time points given in \code{Time}, else, it applies it to the range indicated by \code{Time}.
#' @return MeasureSweeps: A \linkS4class{PRecording} or \linkS4class{PCollection}, respectiveley with an updated MetaData Slot or, if \code{ReturnPMObject=F} a matrix.
#' @exportMethod MeasureSweeps
setGeneric(name="MeasureSweeps",
           def=function(X,
                        Trace,
                        Sweeps=GetSweepNames(X),
                        Time,
                        label,
                        FUN=mean,
                        ReturnPMObject=T)
           {
             standardGeneric("MeasureSweeps")
           }
)

#' @describeIn Measure Method for PRecording
setMethod("MeasureSweeps",
          "PRecording",
          function(X,
                   Trace,
                   Sweeps=GetSweepNames(X),
                   Time,
                   label,
                   FUN=mean,
                   ReturnPMObject=T
          ){
            if(length(Trace)>1){
              stop("This function can only be applied to a single Trace")
            }
            if(as.character(substitute(FUN)) %in% c("+","-","+","/","^","**")){
              message(paste("Binary operator", as.character(substitute(FUN)), "applied. Use Time vector as exclusive points."))
              TimeExclusive=T
            }else{
              TimeExclusive=F
            }
            out<-apply(GetData(X,
                                  Traces=Trace,
                                  Time=Time,
                                  Sweeps=Sweeps,
                                  nowarnings = T,
                                  TimeExclusive=TimeExclusive),
                       "Time",
                       FUN=FUN)

            if (!ReturnPMObject){
              rownames(out)<-paste0(label,".",Sweeps)
              return(out)
            }else{
              return(AddMetaData(X,
                                 out,
                                 title=label)
              )
            }
          }
)

#' @describeIn Measure Method for PCollection
setMethod("MeasureSweeps",
          "PCollection",
          function(X,
                   Trace,
                   Sweeps=GetSweepNames(X),
                   Time,
                   label,
                   FUN=mean,
                   ReturnPMObject=T
          ){
            if(length(Trace)>1){
              stop("This function can only be applied to a single Trace")
            }
            X<-lapply(X,
                           function(x){
                             MeasureSweeps(x,
                                           Trace=Trace,
                                           Time=Time,
                                           Sweeps=Sweeps,
                                           label=label,
                                           FUN=FUN,
                                           ReturnPMObject = T)
                           },
                           ReturnPMObject=T
            )

            out<-lapply(X,function(x){GetMetaData(x,label)[,2]})

            if(length(Sweeps)>1){
              label<-paste0(label,".",Sweeps)
            }
            colnames(out)<-label
            if (!ReturnPMObject){
              out<-as.matrix(out)
              colnames(out)<-label
              rownames(out)<-X@Names
              return(out)
            }else{
              return(AddMetaData(X,
                                 out,
                                 title=label)
              )
            }
          }
)

#' @describeIn Measure This is a convenience method providing easy to process data.frames for generation of time series and dose-response curves
#' @return MeasureStimResp: A a \link[base:data.frame]{data.frame} with five columns: "Name","Group","Stimulus","StimTimes","Response"
#' @exportMethod MeasureStimResp
setGeneric(name="MeasureStimResp",
           def=function(X,
                        StimTrace="V-mon",
                        RespTrace="I-mon",
                        Time,
                        FUN=mean)
           {
             standardGeneric("MeasureStimResp")
           }
)

#' @describeIn Measure Method for PRecording
setMethod("MeasureStimResp",
          "PRecording",
          function(X,
                   StimTrace="V-mon",
                   RespTrace="I-mon",
                   Time,
                   FUN=mean){

            stim<-MeasureSweeps(X,
                                  Trace=StimTrace,
                                  Sweeps=GetSweepNames(X),
                                  Time,
                                  label="Stimulus",
                                  FUN=FUN,
                                  ReturnPMObject=F)
            resp<-MeasureSweeps(X,
                                  Trace=RespTrace,
                                  Sweeps=GetSweepNames(X),
                                  Time,
                                  label="Response",
                                  FUN=FUN,
                                  ReturnPMObject=F)
            out<-as.data.frame(cbind(stim,GetSweepTimes(X)-min(GetSweepTimes(X)),resp))
            out<-cbind<-cbind(GetSweepNames(X),out)
            colnames(out)<-c("Name","Stimulus","StimTimes","Response")
            out
          }
)

#' @describeIn Measure Method for PCollection
#' @importFrom tidyr pivot_longer
#' @importFrom dplyr left_join
setMethod("MeasureStimResp",
          "PCollection",
          function(X,
                   StimTrace="V-mon",
                   RespTrace="I-mon",
                   Time,
                   FUN=mean){

            stim<-t(MeasureSweeps(GetData(X,Series=X@Names[1]),
                                  Trace=StimTrace,
                                  Sweeps=GetSweepNames(X),
                                  Time,
                                  label="Stimulus",
                                  FUN=FUN,
                                  ReturnPMObject=F))
            resp<-t(MeasureSweeps(X,
                                  Trace=RespTrace,
                                  Sweeps=GetSweepNames(X),
                                  Time,
                                  label="Response",
                                  FUN=FUN,
                                  ReturnPMObject=F))
            out<-as.data.frame(cbind(stim,GetSweepTimes(X)-min(GetSweepTimes(X)),resp))
            colnames(out)<-c("Stimulus","StimTimes",X@Names)
            out<-pivot_longer(out,X@Names)
            colnames(out)<-c("Stimulus","StimTimes","Name","Response")
            groups<-as.data.frame(cbind(X@Names,as.character(X@Group)))
            colnames(groups)<-c("Name","Group")
            groups$Name<-as.factor(groups$Name)
            groups$Group<-as.factor(groups$Group)
            out<-as.data.frame(left_join(out,groups,by="Name",copy=T))
            out[, c("Name","Group","Stimulus","StimTimes","Response")]
            out
          }
)

