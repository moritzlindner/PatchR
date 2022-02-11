#' Plotting methods
#'
#' These methods create "typical" ephsy graphs like dose-response curves, time-series or point statistics.
#'
#' @inheritParams MeasureStimResp
#' @param Sweep Sweep to analyse for group comparison
#' @param fun Function to apply on graph for stimulus response plotting
#' @return A \linkS4class{PCollection} with an item added to the Plots slot if \code{ReturnPMObject=T} or a \link[ggplot2:ggplot]{ggplot}.
#' @name Plot
NULL

#' @describeIn Plot This method builds a dose-response curve
#' @exportMethod PlotStimResp
setGeneric(name="PlotStimResp",
           def=function(X,
                        StimTrace="V-mon",
                        RespTrace="I-mon",
                        Time,
                        fun=mean,
                        ReturnPMObject=T)
           {
             standardGeneric("PlotStimResp")
           }
)

#' @describeIn Plot Method for PRecording
setMethod("PlotStimResp",
          "PRecording",
          function(X,
                   StimTrace="V-mon",
                   RespTrace="I-mon",
                   Time,
                   fun=mean,
                   ReturnPMObject=T){
            PlotStimRespgeneric(X,
                               StimTrace,
                               RespTrace,
                               Time,
                               fun,
                               ReturnPMObject)
          }
)
#' @describeIn Plot Method for PCollection
setMethod("PlotStimResp",
          "PCollection",
          function(X,
                   StimTrace="V-mon",
                   RespTrace="I-mon",
                   Time,
                   fun=mean,
                   ReturnPMObject=T){
            PlotStimRespgeneric(X,
                               StimTrace,
                               RespTrace,
                               Time,
                               fun,
                               ReturnPMObject)
          }
          )


#' @noRd
PlotStimRespgeneric<-function(X,
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
    StimUnit<-paste0(ConvenientScalessi(dat$Stimulus),X@Units[GetTraceNames(X)==StimTrace])
    RespUnit<-paste0(ConvenientScalessi(dat$Response),X@Units[GetTraceNames(X)==RespTrace])
  }else{
    StimUnit<-paste0(ConvenientScalessi(dat$Stimulus),X@Series[[1]]@Units[GetTraceNames(X@Series[[1]])==StimTrace])
    RespUnit<-paste0(ConvenientScalessi(dat$Response),X@Series[[1]]@Units[GetTraceNames(X@Series[[1]])==RespTrace])
  }
  dat$Stimulus<-ConvenientScalesvalue(dat$Stimulus)
  dat$Response<-ConvenientScalesvalue(dat$Response)

  if(!("Group" %in% colnames(dat))){
    dat$Group<-"Genereic"
  }
  out<-ggplot2::ggplot(dat,ggplot2::aes_string(y="Response",x="Stimulus",colour="Group"))+
    ggplot2::stat_summary(fun = mean, geom="line")+
    ggplot2::stat_summary(fun = mean, geom="point")+
    ggplot2::stat_summary(fun.data = mean_se, geom="errorbar")+
    ggplot2::theme_classic()+
    ggplot2::theme(legend.position = "bottom",
          text = element_text(size=8),
          strip.background = ggplot2::element_rect(fill = "light grey",colour = NULL, size=0))+
    ggplot2::xlab(paste(StimTrace," [",StimUnit,"]"))+
    ggplot2::ylab(paste(RespTrace," [",RespUnit,"]"))
  if(ReturnPMObject){
    X@Plots[["DoseResp_Plot"]]<-out
    X
  }else{
    out
  }
}

#' @describeIn Plot This method builds a time Series plot
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

#' @describeIn Plot Method for PRecording
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
#' @describeIn Plot Method for PCollection
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
    TimeUnit<-paste0(ConvenientScalessi(dat$StimTimes),X@TimeUnit)
    RespUnit<-paste0(ConvenientScalessi(dat$Response),X@Units[GetTraceNames(X)==RespTrace])
  }else{
    TimeUnit<-paste0(ConvenientScalessi(dat$StimTimes),X@Series[[1]]@TimeUnit)
    RespUnit<-paste0(ConvenientScalessi(dat$Response),X@Series[[1]]@Units[GetTraceNames(X@Series[[1]])==RespTrace])

  }
  dat$StimTimes<-ConvenientScalesvalue(dat$StimTimes)
  dat$Response<-ConvenientScalesvalue(dat$Response)

  if(!("Group" %in% colnames(dat))){
    dat$Group<-"Genereic"
  }
  out<-ggplot2::ggplot(dat,ggplot2::aes_string(y="Response",x="StimTimes",colour="Group"))+
    ggplot2::stat_summary(fun = mean, geom="line")+
    ggplot2::stat_summary(fun = mean, geom="point")+
    ggplot2::stat_summary(fun.data = mean_se, geom="errorbar")+
    ggplot2::theme_classic()+
    ggplot2::theme(legend.position = "bottom",
          text = element_text(size=8),
          strip.background = ggplot2::element_rect(fill = "light grey",colour = NULL, size=0))+
    ggplot2::xlab(paste("Time [",TimeUnit,"]"))+
    ggplot2::ylab(paste(RespTrace," [",RespUnit,"]"))
  if(ReturnPMObject){
    X@Plots[["Time_Plot"]]<-out
    X
  }else{
    out
  }
}

#' @describeIn Plot This method builds a boxplot for comparison between groups as stored in the \linkS4class{PCollection}.
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
#' @describeIn Plot Method for PCollection
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
    RespUnit<-paste0(ConvenientScalessi(dat$Response),X@Units[GetTraceNames(X)==RespTrace])
  }else{
    RespUnit<-paste0(ConvenientScalessi(dat$Response),X@Series[[1]]@Units[GetTraceNames(X@Series[[1]])==RespTrace])
  }
  dat$Response<-ConvenientScalesvalue(dat$Response)

  if(!("Group" %in% colnames(dat))){
    dat$Group<-"Genereic"
  }

  message("Summary statistics")
  print(desc_statby(dat,"Response","Group"))
  message("Group comparison")
  print(compare_means(Response ~ Group,dat))

  out<-ggplot(dat,ggplot2::aes_string(y="Response",x="Group"))+
    ggplot2::geom_boxplot()+
    ggplot2::geom_point(position = "jitter")+
    ggpubr::stat_compare_means()+
    ggplot2::theme_classic()+
    ggplot2::theme(legend.position = "bottom",
          text = element_text(size=8),
          strip.background = ggplot2::element_rect(fill = "light grey",colour = NULL, size=0))+
    ggplot2::ylab(paste(RespTrace," [",RespUnit,"]"))
  if(ReturnPMObject){
    X@Plots[["GroupComparison_Plot"]]<-out
    X
  }else{
    out
  }
}
