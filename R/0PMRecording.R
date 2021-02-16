validPMRecording<-function(object) {
  ret=0
  if (!(length(object@Traces) == length(object@Data))){
    ret<-ret+1
    print("Monitor name list incompatible to items in Data")
  }
  if (!(length(object@Traces) == length(object@Units))){
    ret<-ret+1
    print("Monitor name list incompatible to items in Units")
  }
  if (!(length(object@Data)>0)){
    ret<-ret+1
    print("No data added")
  }
  if (!(all(unlist(lapply(object@Data,function(x){is.matrix(x)}))))){
    ret<-ret+1
    print("Data not in matrix format")
  }
  if( length(object@Data)>1){
    s<-var(unlist(lapply(object@Data,function(x){dim(x)[1]})))
    s[is.na(s)] <- 0
    if (!(s==0)){
      ret+1
      print("Traces have unequal dimensions")
    }
    s<-var(unlist(lapply(object@Data,function(x){dim(x)[2]})))
    s[is.na(s)] <- 0
    if (!(s==0)){
      ret<-ret+1
      print("Traces have unequal dimensions")
    }
  }
  if (!(length(object@getTimeTrace) == dim(object@Data[[1]])[1])){
    ret<-ret+1
    print("Time trace inconsitent to data")
  }
  if (!(length(object@Sweeps) == dim(object@Data[[1]])[2])){
    ret+1
    print("Sweep names inconsitent to Data")
  }
  if (!(length(object@Sweeps) == length(object@SweepTimes))){
    ret<-ret+1
    print("incompatible sweep ids and timing data")

  }
  if (!(all(object@RecordingParams@Traces %in% object@Traces))){
    ret<-ret+1
    print("Incompatible Trace list")
  }
  if(!all(dim(object@MetaData)==0)){
    if (!(dim(object@MetaData)[1] == length(object@Sweeps))){
      ret<-ret+1
      print("MetaData has not the same length as there are Sweeps")
    }
  }
  if(ret==0) {TRUE} else {FALSE}
}


#' S4 class storing imported [Patch Master](https://www.heka.com/downloads/downloads_main.html#down_patchmaster)  Traces.
#'
#' This class stores imported [Patch Master](https://www.heka.com/downloads/downloads_main.html#down_patchmaster)  Traces. Currently only checked for time series. It is strictly validated to assure data consistency
#'
#' \describe{
#'    \item{Traces}{Character vector containing names of the Traces (=Monitors) imported form the dat file and any subsequently computed Trace. Computed Traces have the same dimensions as imported}
#'
#'    \item{Units}{Character vector containing the SI units for data stored in corresponding Trace. Order as in Traces.}
#'
#'    \item{getTimeTrace}{Numeric vector containing the time points of the recording.}
#'
#'    \item{Sweeps}{Ordered vector containing the names of the sweeps.}
#'
#'    \item{SweepTimes}{Numeric vector containing start times corresponding to Sweeps}
#'
#'    \item{Data}{List of matrices. One list item per Trace. Matrix rows correspond to getTimeTrace, columns to Sweeps}
#'
#'    \item{Plots}{List that can contain any ggplot derived from the data. List item names that equal Traces are reserved.}
#'
#'    \item{MetaData}{matrix that can contain additional per-sweep Metadata. Per-time Metadata can be stored in the Data slot using the \link[=addTrace]{addTrace} method.}
#'
#'    \item{RecordingParams}{An item of class PMRecordingParams containing recording parameters for that trace.}
#'  }
#' @exportClass PMRecording
PMRecording<-setClass(Class="PMRecording",
                  slots =  list(Traces="character",
                                Units="character",
                                getTimeTrace="numeric",
                                TimeUnit="character",
                                Sweeps="ordered", # was character
                                SweepTimes="numeric",
                                Data="list",
                                MetaData="matrix",
                                .MetaDataFx="list",
                                Plots="list",
                                #Computed="list",
                                RecordingParams="PMRecordingParams"),
                  prototype=list(
                    MetaData = matrix(ncol=0,nrow=0),
                    .MetaDataFx = list(),
                    Plots = list()
                  ),
                  validity = validPMRecording
)
