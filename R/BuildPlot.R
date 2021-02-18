#' TBC
#'
#' This function creates a basic visualization of quality-relevant recording parameters in the PMCollection These will be stored in the Plots slot and can be accessed using \link[=Inspect]{Inspect}(X,"QC_Metrics")
#'
#' @inheritParams MeasureStimResp
#' @param Sweep Sweep to analyse for group comparison
#' @return A \linkS4class{PMCollection} with an item added to the Plots slot if \code{ReturnPMObject=T} or a \link[=ggplot2::ggplot]{ggplot}.
#' @name Plots
NULL

#' @describeIn Plots This method builds a dose-response curve
#' @exportMethod DoseRespPlot
setGeneric(name="DoseRespPlot",
           def=function(X,
                        StimTrace="V-mon",
                        RespTrace="I-mon",
                        Time,
                        fun=mean,
                        ReturnPMObject=T)
           {
             standardGeneric("DoseRespPlot")
           }
)

setMethod("DoseRespPlot",
          "PMRecording",
          function(X,
                   StimTrace="V-mon",
                   RespTrace="I-mon",
                   Time,
                   fun=mean,
                   ReturnPMObject=T){
            DoseRespPlotgeneric(X,
                               StimTrace,
                               RespTrace,
                               Time,
                               fun,
                               ReturnPMObject)
          }
          )
setMethod("DoseRespPlot",
          "PMCollection",
          function(X,
                   StimTrace="V-mon",
                   RespTrace="I-mon",
                   Time,
                   fun=mean,
                   ReturnPMObject=T){
            DoseRespPlotgeneric(X,
                               StimTrace,
                               RespTrace,
                               Time,
                               fun,
                               ReturnPMObject)
          }
          )

DoseRespPlotgeneric<-function(X,
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

  dat$Stimulus<-convenientScalesvalue(dat$Stimulus)
  dat$Response<-convenientScalesvalue(dat$Response)

  StimUnit<-paste0(convenientScalessi(dat$Stimulus),X@Units[getTraceNames(X)==StimTrace])
  RespUnit<-paste0(convenientScalessi(dat$Response),X@Units[getTraceNames(X)==RespTrace])

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
    xlab(paste(StimTrace," [",X@Series[[1]]@Units[getTraceNames(X@Series[[1]])==StimTrace],"]"))+
    ylab(paste(RespTrace," [",X@Series[[1]]@Units[getTraceNames(X@Series[[1]])==RespTrace],"]"))
  if(ReturnPMObject){
    X@Plots[["DoseResp_Plot"]]<-out
  }else{
    out
  }
}

#' @describeIn Plots This method builds a time Series plot
#' @exportMethod TimeSeriesPlot
setGeneric(name="TimeSeriesPlot",
           def=function(X,
                        RespTrace="I-mon",
                        Time,
                        fun=mean,
                        ReturnPMObject=T)
           {
             standardGeneric("TimeSeriesPlot")
           }
)
setMethod("TimeSeriesPlot",
          "PMRecording",
          function(X,
                   RespTrace="I-mon",
                   Time,
                   fun=mean,
                   ReturnPMObject=T){
            DoseRespPlotgeneric(X,
                                StimTrace,
                                RespTrace,
                                Time,
                                fun,
                                ReturnPMObject)
          }
)
setMethod("TimeSeriesPlot",
          "PMCollection",
          function(X,
                   RespTrace="I-mon",
                   Time,
                   fun=mean,
                   ReturnPMObject=T){
            DoseRespPlotgeneric(X,
                                StimTrace,
                                RespTrace,
                                Time,
                                fun,
                                ReturnPMObject)
          }
)
TimeSeriesPlotgeneric<-function(X,
                             RespTrace="I-mon",
                             Time,
                             fun=mean,
                             ReturnPMObject=T){
  dat<-MeasureStimResp(X,
                       StimTrace=RespTrace,
                       RespTrace,
                       Time,
                       fun)

  dat$StimTimes<-convenientScalesvalue(dat$StimTimes)
  dat$Response<-convenientScalesvalue(dat$Response)

  TimeUnit<-paste0(convenientScalessi(dat$StimTimes),X@TimeUnit)
  RespUnit<-paste0(convenientScalessi(dat$Response),X@Units[getTraceNames(X)==RespTrace])

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
    xlab(paste("Time [",X@Series[[1]]@Units[getTraceNames(X@Series[[1]])==StimTrace],"]"))+
    ylab(paste(RespTrace," [",X@Series[[1]]@Units[getTraceNames(X@Series[[1]])==RespTrace],"]"))
  if(ReturnPMObject){
    X@Plots[["Time_Plot"]]<-out
  }else{
    out
  }
}

#' @describeIn Plots This method builds a boxplot for comparison between groups as stored in the \linkS4class{PMCollection}.
#' @exportMethod GroupComparisonPlot
setGeneric(name="GroupComparisonPlot",
           def=function(X,
                        Sweep,
                        RespTrace="I-mon",
                        Time,
                        fun=mean,
                        ReturnPMObject=T)
           {
             standardGeneric("GroupComparisonPlot")
           }
)
setMethod("GroupComparisonPlot",
          "PMCollection",
          function(X,
                   RespTrace="I-mon",
                   Time,
                   fun=mean,
                   ReturnPMObject=T){
            DoseRespPlotgeneric(X,
                                Sweep,
                                StimTrace,
                                RespTrace,
                                Time,
                                fun,
                                ReturnPMObject)
          }
)
#' @importFrom ggpubr stat_compare_means desc_statby compare_means
GroupComparisonPlotgeneric<-function(X,
                           Sweep,
                           RespTrace="I-mon",
                           Time,
                           fun=mean,
                           ReturnPMObject=T){
  X<-SubsetData(X,Sweeps=Sweep)
  dat<-MeasureStimResp(X,
                       StimTrace=RespTrace,
                       RespTrace,
                       Time,
                       fun)

  dat$Response<-convenientScalesvalue(dat$Response)

  RespUnit<-paste0(convenientScalessi(dat$Response),X@Units[getTraceNames(X)==RespTrace])

  if(!("Group" %in% colnames(dat))){
    dat$Group<-"Genereic"
  }

  print(desc_statby(dat,"Response","Group"))
  print(compare_means(Response ~ Group,dat))

  out<-ggplot(dat,aes(y=Response,x=Group))+
    geom_boxplot()+
    stat_compare_means()
    theme_classic()+
    theme(legend.position = "bottom",
          text = element_text(size=8),
          strip.background = element_rect(fill = "light grey",colour = NULL, size=0))+
    ylab(paste(RespTrace," [",X@Series[[1]]@Units[getTraceNames(X@Series[[1]])==RespTrace],"]"))
  if(ReturnPMObject){
    X@Plots[["GroupComparison_Plot"]]<-out
  }else{
    out
  }
}
