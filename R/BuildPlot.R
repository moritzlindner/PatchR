#' Plotting methods
#'
#' These methods create "typical" ephsy graphs like dose-response curves, time-series or point statistics.
#'
#' @inheritParams MeasureStimResp
#' @param Sweep Sweep to analyse for group comparison
#' @return A \linkS4class{PCollection} with an item added to the Plots slot if \code{ReturnPMObject=T} or a \link[=ggplot2::ggplot]{ggplot}.
#' @name Plots
NULL

#' @describeIn Plots This method builds a dose-response curve
#' @exportMethod PlotDoseResp
setGeneric(name="PlotDoseResp",
           def=function(X,
                        StimTrace="V-mon",
                        RespTrace="I-mon",
                        Time,
                        fun=mean,
                        ReturnPMObject=T)
           {
             standardGeneric("PlotDoseResp")
           }
)

setMethod("PlotDoseResp",
          "PRecording",
          function(X,
                   StimTrace="V-mon",
                   RespTrace="I-mon",
                   Time,
                   fun=mean,
                   ReturnPMObject=T){
            PlotDoseRespgeneric(X,
                               StimTrace,
                               RespTrace,
                               Time,
                               fun,
                               ReturnPMObject)
          }
          )
setMethod("PlotDoseResp",
          "PCollection",
          function(X,
                   StimTrace="V-mon",
                   RespTrace="I-mon",
                   Time,
                   fun=mean,
                   ReturnPMObject=T){
            PlotDoseRespgeneric(X,
                               StimTrace,
                               RespTrace,
                               Time,
                               fun,
                               ReturnPMObject)
          }
          )

PlotDoseRespgeneric<-function(X,
                             StimTrace="V-mon",
                             RespTrace="I-mon",
                             Time,
                             fun=mean,
                             ReturnPMObject=T){
  dat<-MeasureStimResp(X,
                       StimTrace,
                       RespTrace,
                       Time,
                       fun)

  if(class(X)[1]=="PRecording"){
    StimUnit<-paste0(convenientScalessi(dat$Stimulus),X@Units[GetTraceNames(X)==StimTrace])
    RespUnit<-paste0(convenientScalessi(dat$Response),X@Units[GetTraceNames(X)==RespTrace])
  }else{
    StimUnit<-paste0(convenientScalessi(dat$Stimulus),X@Series[[1]]@Units[GetTraceNames(X@Series[[1]])==StimTrace])
    RespUnit<-paste0(convenientScalessi(dat$Response),X@Series[[1]]@Units[GetTraceNames(X@Series[[1]])==RespTrace])
  }
  dat$Stimulus<-convenientScalesvalue(dat$Stimulus)
  dat$Response<-convenientScalesvalue(dat$Response)

  if(!("Group" %in% colnames(dat))){
    dat$Group<-"Genereic"
  }
  out<-ggplot(dat,aes(y=Response,x=Stimulus,colour=Group))+
    stat_summary(fun = mean, geom="line")+
    stat_summary(fun = mean, geom="point")+
    stat_summary(fun.data = mean_se, geom="errorbar")+
    theme_classic()+
    theme(legend.position = "bottom",
          text = element_text(size=8),
          strip.background = element_rect(fill = "light grey",colour = NULL, size=0))+
    xlab(paste(StimTrace," [",StimUnit,"]"))+
    ylab(paste(RespTrace," [",RespUnit,"]"))
  if(ReturnPMObject){
    X@Plots[["DoseResp_Plot"]]<-out
    X
  }else{
    out
  }
}

#' @describeIn Plots This method builds a time Series plot
#' @exportMethod PlotTimeSeries
setGeneric(name="PlotTimeSeries",
           def=function(X,
                        RespTrace="I-mon",
                        Time,
                        fun=mean,
                        ReturnPMObject=T)
           {
             standardGeneric("PlotTimeSeries")
           }
)
setMethod("PlotTimeSeries",
          "PRecording",
          function(X,
                   RespTrace="I-mon",
                   Time,
                   fun=mean,
                   ReturnPMObject=T){
            PlotTimeSeriesgeneric(X,
                                RespTrace,
                                Time,
                                fun,
                                ReturnPMObject)
          }
)
setMethod("PlotTimeSeries",
          "PCollection",
          function(X,
                   RespTrace="I-mon",
                   Time,
                   fun=mean,
                   ReturnPMObject=T){
            PlotTimeSeriesgeneric(X=X,
                           RespTrace=RespTrace,
                           Time=Time,
                           fun=fun,
                           ReturnPMObject=ReturnPMObject)
          }
)
PlotTimeSeriesgeneric<-function(X,
                             RespTrace,
                             Time,
                             fun,
                             ReturnPMObject){
  dat<-MeasureStimResp(X=X,
                       StimTrace=RespTrace,
                       RespTrace=RespTrace,
                       Time=Time,
                       FUN=fun)
  if(class(X)[1]=="PRecording"){
    TimeUnit<-paste0(convenientScalessi(dat$StimTimes),X@TimeUnit)
    RespUnit<-paste0(convenientScalessi(dat$Response),X@Units[GetTraceNames(X)==RespTrace])
  }else{
    TimeUnit<-paste0(convenientScalessi(dat$StimTimes),X@Series[[1]]@TimeUnit)
    RespUnit<-paste0(convenientScalessi(dat$Response),X@Series[[1]]@Units[GetTraceNames(X@Series[[1]])==RespTrace])

  }
  dat$StimTimes<-convenientScalesvalue(dat$StimTimes)
  dat$Response<-convenientScalesvalue(dat$Response)

  if(!("Group" %in% colnames(dat))){
    dat$Group<-"Genereic"
  }
  out<-ggplot(dat,aes(y=Response,x=StimTimes,colour=Group))+
    stat_summary(fun = mean, geom="line")+
    stat_summary(fun = mean, geom="point")+
    stat_summary(fun.data = mean_se, geom="errorbar")+
    theme_classic()+
    theme(legend.position = "bottom",
          text = element_text(size=8),
          strip.background = element_rect(fill = "light grey",colour = NULL, size=0))+
    xlab(paste("Time [",TimeUnit,"]"))+
    ylab(paste(RespTrace," [",RespUnit,"]"))
  if(ReturnPMObject){
    X@Plots[["Time_Plot"]]<-out
    X
  }else{
    out
  }
}

#' @describeIn Plots This method builds a boxplot for comparison between groups as stored in the \linkS4class{PCollection}.
#' @exportMethod PlotGroupComparison
setGeneric(name="PlotGroupComparison",
           def=function(X,
                        Sweep,
                        RespTrace="I-mon",
                        Time,
                        fun=mean,
                        ReturnPMObject=T)
           {
             standardGeneric("PlotGroupComparison")
           }
)
setMethod("PlotGroupComparison",
          "PCollection",
          function(X,
                   Sweep,
                   RespTrace="I-mon",
                   Time,
                   fun=mean,
                   ReturnPMObject=T){
            PlotGroupComparisongeneric(X,
                                Sweep,
                                RespTrace,
                                Time,
                                fun,
                                ReturnPMObject)
          }
)
#' @importFrom ggpubr stat_compare_means desc_statby compare_means
PlotGroupComparisongeneric<-function(X,
                           Sweep,
                           RespTrace="I-mon",
                           Time,
                           fun=mean,
                           ReturnPMObject){
  X<-GetData(X,Sweeps=Sweep)
  dat<-MeasureStimResp(X,
                       StimTrace=RespTrace,
                       RespTrace,
                       Time,
                       fun)

  if(class(X)[1]=="PRecording"){
    RespUnit<-paste0(convenientScalessi(dat$Response),X@Units[GetTraceNames(X)==RespTrace])
  }else{
    RespUnit<-paste0(convenientScalessi(dat$Response),X@Series[[1]]@Units[GetTraceNames(X@Series[[1]])==RespTrace])
  }
  dat$Response<-convenientScalesvalue(dat$Response)

  if(!("Group" %in% colnames(dat))){
    dat$Group<-"Genereic"
  }

  message("Summary statistics")
  print(desc_statby(dat,"Response","Group"))
  message("Group comparison")
  print(compare_means(Response ~ Group,dat))

  out<-ggplot(dat,aes(y=Response,x=Group))+
    geom_boxplot()+
    geom_point(position = "jitter")+
    stat_compare_means()+
    theme_classic()+
    theme(legend.position = "bottom",
          text = element_text(size=8),
          strip.background = element_rect(fill = "light grey",colour = NULL, size=0))+
    ylab(paste(RespTrace," [",RespUnit,"]"))
  if(ReturnPMObject){
    X@Plots[["GroupComparison_Plot"]]<-out
    X
  }else{
    out
  }
}
