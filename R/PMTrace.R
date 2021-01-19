validPMTrace<-function(object) {
  ret=0
  if (!(length(object@Traces) == length(object@Data))){
    ret+1
    print("Monitor name list incompatible to items in Data")
  }
  if (!(length(object@Traces) == length(object@Units))){
    ret+1
    print("Monitor name list incompatible to items in Units")
  }
  if (!(length(object@Data)>0)){
    ret+1
    print("No data added")
  }
  if (!(all(unlist(lapply(object@Data,function(x){is.matrix(x)}))))){
    ret+1
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
      ret+1
      print("Traces have unequal dimensions")
    }
  }
  if (!(length(object@TimeTrace) == dim(object@Data[[1]])[1])){
    ret+1
    print("Time trace inconsitent to data")
  }
  if (!(length(object@Sweeps) == dim(object@Data[[1]])[2])){
    ret+1
    print("Sweep names inconsitent to Data")
  }
  if (!(length(object@Sweeps) == length(object@SweepTimes))){
    ret+1
    print("incompatible sweep ids and timing data")

  }
  if (!(all(object@RecordingParams@Traces %in% object@Traces))){
    ret+1
    print("Incompatible Trace list")
  }
  if(ret==0) {TRUE} else {FALSE}
}


#' S4 class storing imported PatchMaster Traces.
#'
#' This class stores imported PatchMaster Traces. Currently only checked for time series. It is strictly validated to assure data consistency
#'
#' \describe{
#'    \item{Traces}{Character vector containing names of the Traces (=Monitors) imported form the dat file and any subsequently computed Trace. Computed Traces have the same dimensions as imported}
#'
#'    \item{Units}{Character vector containing the SI units for data stored in corresponding Trace. Order as in Traces.}
#'
#'    \item{TimeTrace}{Numeric vector containing the time points of the recording.}
#'
#'    \item{Sweeps}{Ordered vector containing the names of the sweeps.}
#'
#'    \item{SweepTimes}{Numeric vector containing start times corresponding to Sweeps}
#'
#'    \item{Data}{List of matrices. One list item per Trace. Matrix rows correspond to TimeTrace, columns to Sweeps}
#'
#'    \item{Plots}{List that can contain any ggplot derived from the data. List item names that equal Traces are reserved.}
#'
#'    \item{RecordingParams}{An item of class PMRecordingParams containing recording parameters for that trace.}
#'  }
#' @exportClass PMTrace
PMTrace<-setClass(Class="PMTrace",
                  slots =  list(Traces="character",
                                Units="character",
                                TimeTrace="double",
                                TimeUnit="character",
                                Sweeps="ordered", # was character
                                SweepTimes="double",
                                Data="list",
                                Plots="list",
                                RecordingParams="PMRecordingParams"),
                  validity = validPMTrace
)
