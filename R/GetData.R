#' @describeIn Get This function subsets objects by \var{Trace}, \var{Sweep} or \var{Time}. For  \linkS4class{PCollection} additionally by \var{Series} or \var{Group}
#' @param Traces,Sweeps List of Traces/Sweeps to keep
#' @param Time either a range of time points to keep, or, if \code{TimeExclusive} is \code{TRUE}, then two particular time points
#' @param Series Subset by Series name. Only for  \linkS4class{PCollection} .
#' @param Group Subset by Group name. Only for  \linkS4class{PCollection} .
#' @param TimeExclusive Keep only the two time points stated under Time, not the range
#' @param nowarnings Supress warning messages.
#' @return A \linkS4class{PRecording} or \linkS4class{PCollection} object (for \code{GetData})
#' @exportMethod GetData
setGeneric(name="GetData",
           def=function(X,
                        Traces=GetTraceNames(X),
                        Sweeps=GetSweepNames(X),
                        Time=range(GetTimeTrace(X)),
                        Series=NULL,
                        Group=NULL,
                        TimeExclusive=F,
                        nowarnings=F)
           {
             standardGeneric("GetData")
           }
)

#' @describeIn Get Method for PRecording
setMethod("GetData",
          "PRecording",
          function(X,
                   Traces=GetTraceNames(X),
                   Sweeps=GetSweepNames(X),
                   Time=range(GetTimeTrace(X)),
                   TimeExclusive=F,
                   nowarnings=F)
            {
            if(!nowarnings){
              # FIXME !!! Below: only show this if any of that slots is not empty
              warning("Subsetting clears all analysis and plotting slots for data consistency!")
            }
            if(all.equal(Traces, GetTraceNames(X))!=TRUE){
              if(!nowarnings) {cat("Only keep Traces:", Traces,"\n")}
              if(!all(Traces %in% GetTraceNames(X))){
                stop("Traces to subset not in X")
              }
            }
            if(all.equal(Sweeps, GetSweepNames(X))!=TRUE){
              if(!nowarnings) {cat("Only keep Sweeps: ", Sweeps,"\n")}
              if(!all(Sweeps %in% GetSweepNames(X))){
                stop("Traces to subset not in X")
              }
            }
            if(all.equal(Time, range(GetTimeTrace(X)))!=TRUE){
              if(!TimeExclusive){
                if(!nowarnings) {cat("Only keep Times: ", Time[1]," to ", Time[2],"\n")}
                Time<-GetTimeTrace(X)[GetTimeTrace(X)>=Time[1] & GetTimeTrace(X)<=Time[2]]
              }else{ # if extracting exact time points. get closest to values entered
                Time[1]<-GetTimeTrace(X)[which(abs(GetTimeTrace(X)-Time[1])==min(abs(GetTimeTrace(X)-Time[1])))]
                Time[2]<-GetTimeTrace(X)[which(abs(GetTimeTrace(X)-Time[2])==min(abs(GetTimeTrace(X)-Time[2])))]
                cat("Only keep Times: ", Time[1]," and ", length(Time)-1,"others \n")
              }
            }else{
              Time<-GetTimeTrace(X)
            }

            RecordingParams<-X@RecordingParams
            RecordingParams@Traces<-RecordingParams@Traces[RecordingParams@Traces %in% Traces]
            DATA<-list()
            for (i in Traces){
              DATA[[i]]<-as.matrix(X@Data[[i]][GetTimeTrace(X) %in% Time,GetSweepNames(X) %in% Sweeps])
            }
            PRecording(Traces=GetTraceNames(X)[GetTraceNames(X) %in% Traces],
                    Units=X@Units[GetTraceNames(X) %in% Traces],
                    TimeTrace=GetTimeTrace(X)[GetTimeTrace(X) %in% Time],
                    TimeUnit=X@TimeUnit,
                    Sweeps=GetSweepNames(X)[GetSweepNames(X) %in% Sweeps],
                    SweepTimes=X@SweepTimes[GetSweepNames(X) %in% Sweeps],
                    Data=DATA,
                    RecordingParams=RecordingParams
            )
          }
)

#' @describeIn Get Method for PCollection
setMethod("GetData",
          "PCollection",
          function(X,
                   Traces=GetTraceNames(X@Series[[1]]),
                   Sweeps=GetSweepNames(X@Series[[1]]),
                   Time=range(GetTimeTrace(X@Series[[1]])),
                   Series=NULL,
                   Group=NULL,
                   TimeExclusive=F,
                   nowarnings=F)
          {
            X<-lapply(X,function(x) GetData(x,Traces,Sweeps,Time,TimeExclusive,nowarnings=T),ReturnPMObject=T)

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
