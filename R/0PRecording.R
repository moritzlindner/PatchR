validPRecording<-function(object) {
  ret=0
  if (!(length(object@Traces) == length(object@Data))){
    ret<-ret+1
    stop("Trace name list incompatible to items in Data")
  }
  if (!(length(object@Traces) == length(object@Units))){
    ret<-ret+1
    stop("Trace name list incompatible to items in Units")
  }
  if (!(length(object@Data)>0)){
    ret<-ret+1
    stop("No data added")
  }
  if (!(all(unlist(lapply(object@Data,function(x){is.matrix(x)}))))){
    ret<-ret+1
    stop("Data not in matrix format")
  }
  if( length(object@Data)>1){
    s<-stats::var(unlist(lapply(object@Data,function(x){dim(x)[1]})))
    s[is.na(s)] <- 0
    if (!(s==0)){
      ret+1
      stop("Traces have unequal dimensions")
    }
    s<-stats::var(unlist(lapply(object@Data,function(x){dim(x)[2]})))
    s[is.na(s)] <- 0
    if (!(s==0)){
      ret<-ret+1
      stop("Traces have unequal dimensions")
    }
  }
  if (!(length(object@TimeTrace) == dim(object@Data[[1]])[1])){
    ret<-ret+1
    stop("Time trace inconsitent to data")
  }
  if (!(length(object@Sweeps) == dim(object@Data[[1]])[2])){
    ret+1
    stop("Sweep names inconsitent to Data")
  }
  if (!(length(object@Sweeps) == length(object@SweepTimes))){
    ret<-ret+1
    stop("Incompatible sweep ids and timing data")

  }
  if (!(all(object@RecordingParams@Traces %in% object@Traces))){
    ret<-ret+1
    stop("Incompatible Trace list")
  }
  if(!all(dim(object@MetaData)==0)){
    if (!(dim(object@MetaData)[1] == length(object@Sweeps))){
      ret<-ret+1
      stop("MetaData has not the same length as there are Sweeps")
    }
  }
  if(ret==0) {TRUE} else {FALSE}
}


#' S4 class storing imported ePhys traces.
#'
#' `r lifecycle::badge("stable")` \cr
#' This class stores (usually sweep-oriented) electrophysiology data. Currently only import procedures for HEKA's PatchMaster .dat and Axon's .abd files are implemented. It has some strict validity checks implemented to assure data consistency.
#'
#' @slot Traces Character vector containing names of the Traces (=Channels) imported from the dat file and any subsequently computed Trace. Computed Traces have the same dimensions as imported.
#' @slot Units Character vector containing the SI units for data stored in corresponding Trace. Order as in Traces.
#' @slot TimeTrace Numeric vector containing the time points of the recording.
#' @slot Sweeps Ordered vector containing the names of the sweeps.
#' @slot SweepTimes Numeric vector containing start times corresponding to Sweeps.
#' @slot Data List of matrices. One list item per Trace. Matrix rows correspond to TimeTrace, columns to Sweeps.
#' @slot MetaData Matrix that can contain additional per-sweep Metadata. Per-time Metadata can be stored in the Data slot using the \code{\link[=AddTrace]{AddTrace}} method.
#' @slot Plots List that can contain any ggplot derived from the data. List item names that equal Traces are reserved.
#' @slot Fits List that can contain fitted models.
#' @slot RecordingParams An item of class \code{PRecordingParams} containing recording parameters for that trace.
#' @seealso \url{https://www.heka.com/downloads/downloads_main.html#down_patchmaster}
#' @importFrom methods setClass
#' @importFrom methods setMethod
#' @importFrom methods validObject
#' @include 0PRecordingParams.R
#' @exportClass PRecording
PRecording<-setClass(Class="PRecording",
                  slots =  list(Traces="character",
                                Units="character",
                                TimeTrace="numeric",
                                TimeUnit="character",
                                Sweeps="ordered", # was character
                                SweepTimes="numeric",
                                Data="list",
                                MetaData="matrix",
                                .MetaDataFx="list",
                                Plots="list",
                                Fits="list",
                                RecordingParams="PRecordingParams"),
                  prototype=list(
                    MetaData = matrix(ncol=0,nrow=0),
                    .MetaDataFx = list(),
                    Plots = list(),
                    Fits = list()
                  ),
                  validity = validPRecording
)
