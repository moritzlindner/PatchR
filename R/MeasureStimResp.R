setGeneric(name="MeasureStimResp",
           def=function(object,
                        StimTrace="V-mon",
                        RespTrace="I-mon",
                        Time,
                        fun=mean)
           {
             standardGeneric("MeasureStimResp")
           }
)
#' Perform measurements on all recordings in the collection on a per-sweep basis.
#'
#' This function performs measurements on all recordings in the collection for a given trace on a per-sweep basis. This can be useful e.g. for generating data underlying a Time Series Plot.
#'
#' @inheritParams MeasureSweeps
#' @param StimTrace The name of the Trace that contains the Stimulus
#' @param RespTrace The name of the Trace that contains the Response
#' @return A dataframe with four columns: "Name"(factor, PMRecording Name), "Group" (factor), "Stimulus" (numeric), "Response" (numeric)
#' @exportMethod MeasureStimResp
setMethod("MeasureStimResp",
          "PMCollection",
          function(object,
                   StimTrace="V-mon",
                   RespTrace="I-mon",
                   Time,
                   fun=mean){
            stim<-t(MeasureSweeps(SubsetData(object,Series=object@Names[1]),
                                  Trace=StimTrace,
                                  Sweeps=getSweepNames(object),
                                  Time,
                                  label="Stimulus",
                                  fun=fun,
                                  ReturnPMObject=F))
            resp<-t(MeasureSweeps(object,
                                  Trace=RespTrace,
                                  Sweeps=getSweepNames(object),
                                  Time,
                                  label="Response",
                                  fun=fun,
                                  ReturnPMObject=F))

            out<-as.data.frame(cbind(stim,getSweepTimes(object)-min(getSweepTimes(object)),resp))
            colnames(out)<-c("Stimulus","StimTimes",object@Names)
            out<-pivot_longer(out,object@Names)
            colnames(out)<-c("Stimulus","StimTimes","Name","Response")
            groups<-as.data.frame(cbind(object@Names,as.character(object@Group)))
            colnames(groups)<-c("Name","Group")
            groups$Name<-as.factor(groups$Name)
            groups$Group<-as.factor(groups$Group)
            out<-as.data.frame(left_join(out,groups,by="Name",copy=T))
            out[, c("Name","Group","Stimulus","StimTimes","Response")]
            out
          }
)

