setGeneric(name="SubsetData",
           def=function(object,
                        Traces=getTraces(object),
                        Sweeps=getSweeps(object),
                        Time=range(getTimeTrace(object)),
                        Series=NULL,
                        Group=NULL,
                        TimeExclusive=F,
                        nowarnings=F)
           {
             standardGeneric("SubsetData")
           }
)
#' Subset a PMSeries object
#'
#' This function subsets \link[=PMSeries]{PMSeries} or \link[=PMExperiment]{PMExperiment} objects by Trace, Sweep or Time
#'
#' @param object a \link[=PMSeries]{PMSeries} or \link[=PMExperiment]{PMExperiment} object
#' @param Traces,Sweeps List of Traces/Sweeps to keep
#' @param Time either a range of time points to keep, or, if \code{TimeExclusive} is \code{TRUE}, then two particular time points
#' @param Series Subset by Series name. Only for \link[=PMExperiment]{PMExperiment}.
#' @param Group Subset by Group name. Only for \link[=PMExperiment]{PMExperiment}.
#' @param TimeExclusive Keep only the two time points stated under Time, not the range
#' @return A \link[=PMSeries]{PMSeries} object
#' @exportMethod SubsetData
setMethod("SubsetData",
          "PMSeries",
          function(object,
                   Traces=getTraces(object),
                   Sweeps=getSweeps(object),
                   Time=range(getTimeTrace(object)),
                   TimeExclusive=F,
                   nowarnings=F)
            {
            if(!nowarnings){
              # FIXME !!! Below: only show this if any of that slots is not empty
              warning("Subsetting clears all analysis and plotting slots for data consistency!")
            }
            if(all.equal(Traces, getTraces(object))!=TRUE){
              cat("Only keep Traces:", Traces,"\n")
              if(!all(Traces %in% getTraces(object))){
                stop("Traces to subset not in object")
              }
            }
            if(all.equal(Sweeps, getSweeps(object))!=TRUE){
              cat("Only keep Sweeps: ", Sweeps,"\n")
              if(!all(Sweeps %in% getSweeps(object))){
                stop("Traces to subset not in object")
              }
            }
            if(all.equal(Time, range(getTimeTrace(object)))!=TRUE){
              if(!TimeExclusive){
                cat("Only keep Times: ", Time[1]," to ", Time[2],"\n")
                Time<-getTimeTrace(object)[getTimeTrace(object)>=Time[1] & getTimeTrace(object)<=Time[2]]
              }else{ # if extracting exact time points. get closest to values entered
                Time[1]<-getTimeTrace(object)[which(abs(getTimeTrace(object)-Time[1])==min(abs(getTimeTrace(object)-Time[1])))]
                Time[2]<-getTimeTrace(object)[which(abs(getTimeTrace(object)-Time[2])==min(abs(getTimeTrace(object)-Time[2])))]
                cat("Only keep Times: ", Time[1]," and ", length(Time)-1,"others \n")
              }
            }else{
              Time<-getTimeTrace(object)
            }

            RecordingParams<-object@RecordingParams
            RecordingParams@Traces<-RecordingParams@Traces[RecordingParams@Traces %in% Traces]
            DATA<-list()
            for (i in Traces){
              DATA[[i]]<-as.matrix(object@Data[[i]][getTimeTrace(object) %in% Time,getSweeps(object) %in% Sweeps])
            }
            PMSeries(Traces=getTraces(object)[getTraces(object) %in% Traces],
                    Units=object@Units[getTraces(object) %in% Traces],
                    TimeTrace=getTimeTrace(object)[getTimeTrace(object) %in% Time],
                    TimeUnit=object@TimeUnit,
                    Sweeps=getSweeps(object)[getSweeps(object) %in% Sweeps],
                    SweepTimes=object@SweepTimes[getSweeps(object) %in% Sweeps],
                    Data=DATA,
                    RecordingParams=RecordingParams
            )
          }
)

#' @exportMethod SubsetData
setMethod("SubsetData",
          "PMExperiment",
          function(object,
                   Traces=getTraces(object@Series[[1]]),
                   Sweeps=getSweeps(object@Series[[1]]),
                   Time=range(getTimeTrace(object@Series[[1]])),
                   Series=NULL,
                   Group=NULL,
                   TimeExclusive=F,
                   nowarnings=F)
          {
            object<-lapply(object,function(x) SubsetData(x,Traces,Sweeps,Time,TimeExclusive,nowarnings=T),ReturnPMExperiment=T)

            if (!is.null(Group)){
              warning("Plots droped for consistency.")
                keep<-as.character(object@Group) %in% as.character(Group)
                object<-PMExperiment(
                  Series=object@Series[keep],
                  Names=object@Names[keep],
                  Group=object@Group[keep],
                  MetaData=object@MetaData[keep],
                  RecordingParams=object@RecordingParams
                )
            }

            if (!is.null(Series)){
              warning("Plots droped for consistency.")
              if(is.numeric(Series)){
                object<-PMExperiment(
                  Series=object@Series[Series],
                  Names=object@Names[Series],
                  Group=object@Group[Series],
                  MetaData=object@MetaData[Series],
                  RecordingParams=object@RecordingParams
                )
              }else{
                keep<-object@Names %in% Series
                object<-PMExperiment(
                  Series=object@Series[keep],
                  Names=object@Names[keep],
                  Group=object@Group[keep],
                  MetaData=object@MetaData[keep],
                  RecordingParams=object@RecordingParams
                )
              }
            }
            object
          }
)
