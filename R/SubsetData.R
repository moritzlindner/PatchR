setGeneric(name="SubsetData",
           def=function(object,
                        Channels=getChannels(object),
                        Sweeps=getSweeps(object),
                        Time=range(getTimeTrace(object)),
                        TimeExclusive=F)
           {
             standardGeneric("SubsetData")
           }
)
#' Subsetting PMTrace objects
#'
#' This function subsets PMTrace objects by Channel, Sweep or Time
#'
#' @param object a PMTrace object
#' @param Channels,Sweeps List of Channels/Sweeps to keep
#' @param Time either a range of time points to keep, or, if @param TimeExclusive is TRUE, then two particular time points
#' @param TimeExclusive Keep only the two time points stated under Time, not the range
#' @exportMethod SubsetData
setMethod("SubsetData",
          "PMTrace",
          function(object,
                   Channels=getChannels(object),
                   Sweeps=getSweeps(object),
                   Time=range(getTimeTrace(object)
                   ),
                   TimeExclusive=F
          ){
            # FIXME !!! Below: only show this if any of that slots is not empty
            warning("Subsetting clears all analysis and plotting slots for data consistency!")

            if(all.equal(Channels, getChannels(object))!=TRUE){
              s<-cat(Channels)
              print(paste("Only keep Channels:", cat(Channels)))
              if(!all(Channels %in% getChannels(object))){
                stop("Channels to subset not in object")
              }
            }
            if(all.equal(Sweeps, getSweeps(object))!=TRUE){
              s<-cat(Sweeps)
              print(paste("Only keep Sweeps: ", s))
              if(!all(Sweeps %in% getSweeps(object))){
                stop("Channels to subset not in object")
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
            RecordingParams@Channels<-RecordingParams@Channels[RecordingParams@Channels %in% Channels]

            DATA<-list()
            for (i in Channels){
              DATA[[i]]<-object@Data[[i]][getTimeTrace(object) %in% Time,getSweeps(object) %in% Sweeps]
            }
            PMTrace(Channels=getChannels(object)[getChannels(object) %in% Channels],
                    Units=object@Units[getChannels(object) %in% Channels],
                    TimeTrace=getTimeTrace(object)[getTimeTrace(object) %in% Time],
                    TimeUnit=object@TimeUnit,
                    Sweeps=getSweeps(object)[getSweeps(object) %in% Sweeps],
                    SweepTimes=object@SweepTimes[getSweeps(object) %in% Sweeps],
                    Data=DATA,
                    RecordingParams=RecordingParams
            )
          }
)
