#' Subset a PRecording or PCollection object
#'
#' This function subsets \link[=PRecording]{PRecording} or \link[=PCollection]{PCollection} objects by Trace, Sweep or Time
#'
#' @param X a \link[=PRecording]{PRecording} or \link[=PCollection]{PCollection} Object
#' @param Traces,Sweeps List of Traces/Sweeps to keep
#' @param Time either a range of time points to keep, or, if \code{TimeExclusive} is \code{TRUE}, then two particular time points
#' @param Series Subset by Series name. Only for \link[=PCollection]{PCollection}.
#' @param Group Subset by Group name. Only for \link[=PCollection]{PCollection}.
#' @param TimeExclusive Keep only the two time points stated under Time, not the range
#' @return A \link[=PRecording]{PRecording} X
#' @exportMethod SubsetData
setGeneric(name="SubsetData",
           def=function(X,
                        Traces=getTraceNames(X),
                        Sweeps=getSweepNames(X),
                        Time=range(getTimeTrace(X)),
                        Series=NULL,
                        Group=NULL,
                        TimeExclusive=F,
                        nowarnings=F)
           {
             standardGeneric("SubsetData")
           }
)
setMethod("SubsetData",
          "PRecording",
          function(X,
                   Traces=getTraceNames(X),
                   Sweeps=getSweepNames(X),
                   Time=range(getTimeTrace(X)),
                   TimeExclusive=F,
                   nowarnings=F)
            {
            if(!nowarnings){
              # FIXME !!! Below: only show this if any of that slots is not empty
              warning("Subsetting clears all analysis and plotting slots for data consistency!")
            }
            if(all.equal(Traces, getTraceNames(X))!=TRUE){
              if(!nowarnings) {cat("Only keep Traces:", Traces,"\n")}
              if(!all(Traces %in% getTraceNames(X))){
                stop("Traces to subset not in X")
              }
            }
            if(all.equal(Sweeps, getSweepNames(X))!=TRUE){
              if(!nowarnings) {cat("Only keep Sweeps: ", Sweeps,"\n")}
              if(!all(Sweeps %in% getSweepNames(X))){
                stop("Traces to subset not in X")
              }
            }
            if(all.equal(Time, range(getTimeTrace(X)))!=TRUE){
              if(!TimeExclusive){
                if(!nowarnings) {cat("Only keep Times: ", Time[1]," to ", Time[2],"\n")}
                Time<-getTimeTrace(X)[getTimeTrace(X)>=Time[1] & getTimeTrace(X)<=Time[2]]
              }else{ # if extracting exact time points. get closest to values entered
                Time[1]<-getTimeTrace(X)[which(abs(getTimeTrace(X)-Time[1])==min(abs(getTimeTrace(X)-Time[1])))]
                Time[2]<-getTimeTrace(X)[which(abs(getTimeTrace(X)-Time[2])==min(abs(getTimeTrace(X)-Time[2])))]
                cat("Only keep Times: ", Time[1]," and ", length(Time)-1,"others \n")
              }
            }else{
              Time<-getTimeTrace(X)
            }

            RecordingParams<-X@RecordingParams
            RecordingParams@Traces<-RecordingParams@Traces[RecordingParams@Traces %in% Traces]
            DATA<-list()
            for (i in Traces){
              DATA[[i]]<-as.matrix(X@Data[[i]][getTimeTrace(X) %in% Time,getSweepNames(X) %in% Sweeps])
            }
            PRecording(Traces=getTraceNames(X)[getTraceNames(X) %in% Traces],
                    Units=X@Units[getTraceNames(X) %in% Traces],
                    TimeTrace=getTimeTrace(X)[getTimeTrace(X) %in% Time],
                    TimeUnit=X@TimeUnit,
                    Sweeps=getSweepNames(X)[getSweepNames(X) %in% Sweeps],
                    SweepTimes=X@SweepTimes[getSweepNames(X) %in% Sweeps],
                    Data=DATA,
                    RecordingParams=RecordingParams
            )
          }
)

setMethod("SubsetData",
          "PCollection",
          function(X,
                   Traces=getTraceNames(X@Series[[1]]),
                   Sweeps=getSweepNames(X@Series[[1]]),
                   Time=range(getTimeTrace(X@Series[[1]])),
                   Series=NULL,
                   Group=NULL,
                   TimeExclusive=F,
                   nowarnings=F)
          {
            X<-lapply(X,function(x) SubsetData(x,Traces,Sweeps,Time,TimeExclusive,nowarnings=T),ReturnPMObject=T)

            if (!is.null(Group)){
              warning("Plots droped for consistency.")
                keep<-as.character(X@Group) %in% as.character(Group)
                X<-PCollection(
                  Series=X@Series[keep],
                  Names=X@Names[keep],
                  Group=X@Group[keep],
                  MetaData=X@MetaData[keep],
                  RecordingParams=X@RecordingParams
                )
            }

            if (!is.null(Series)){
              warning("Plots droped for consistency.")
              if(is.numeric(Series)){
                md<-matrix(nrow=0,ncol=0)
                try(md<-X@MetaData[Series,],silent=T)
                X<-PCollection(
                  Series=X@Series[Series],
                  Names=X@Names[Series],
                  Group=X@Group[Series],
                  MetaData=md,
                  RecordingParams=X@RecordingParams
                )
              }else{
                keep<-X@Names %in% Series
                md<-matrix(nrow=0,ncol=0)
                try(md<-X@MetaData[keep,],silent=T)
                X<-PCollection(
                  Series=X@Series[keep],
                  Names=X@Names[keep],
                  Group=X@Group[keep],
                  MetaData=md,
                  RecordingParams=X@RecordingParams
                )
              }
            }
            X
          }
)
