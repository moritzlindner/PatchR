validPMCollection<-function(object) {
  ret=0
  if (!(length(object@Series) == length(object@Names))){
    ret<-ret+1
    stop("Name list incompatible to items in Series")
  }
  if(any(duplicated(object@Names))){
    ret<-ret+1
    stop(paste("Name provided not unique: ", object@Names[duplicated(object@Names)]))
  }
  if (!(length(object@Series) == length(object@Group))){
    ret<-ret+1
    stop("Group list length incompatible.")
  }
  if(!all(dim(object@MetaData)==0)){
    if (!(dim(object@MetaData)[1] == length(object@Group))){
      ret<-ret+1
      stop("MetaData incompatible to items in Series")
    }
  }
  if (!all(unlist(lapply(object@Series,function(x) TraceNames(x@RecordingParams))) %in% TraceNames(object@RecordingParams))){
    ret<-ret+1
    print(TraceNames(object@RecordingParams))
    stop("Unequal trace names")
  }

  if (!all(lapply(object@Series,function(x) x@RecordingParams@ProtocolName)==object@RecordingParams@ProtocolName)){
    ret<-ret+1
    stop("Unequal Protocol names")
  }

  if (!all(lapply(object@Series,function(x) x@RecordingParams@RecMode)==object@RecordingParams@RecMode)){
    ret<-ret+1
    stop("Unequal Recording modes")
  }

  if(ret==0) {TRUE} else {FALSE}
}


#' S4 class storing imported [Patch Master](https://www.heka.com/downloads/downloads_main.html#down_patchmaster)  Traces.
#'
#' This class stores imported [Patch Master](https://www.heka.com/downloads/downloads_main.html#down_patchmaster)  Traces. Currently only checked for time series. It is strictly validated to assure data consistency
#'
#' \describe{
#'    \item{Series}{A list containing individual PMRecording objects of identical dimensisons, ProtocolName, RecMode and trace names}
#'    \item{Names}{Name identifier for the individual Series. Unique.}
#'    \item{Group}{Group identifier for the individual Series.}
#'    \item{MetaData}{A matrix with metatdata, e.g. calculated summay stats of each PMRecording object. One row per entry in Series allowed}
#'    \item{Plots}{A slot to store ggplots}
#'    \item{RecordingParams}{Stores the Recording parameters that must be identical for all entries in Series (ProtocolName, RecMode, Traces)}
#'  }
#' @exportClass PMCollection
PMCollection<-setClass(Class="PMCollection",
                  slots =  list(Series="list",
                                Names="character",
                                Group="factor",
                                MetaData="matrix",
                                .MetaDataFx="list",
                                Plots="list",
                                RecordingParams="PMRecordingParams"),
                  prototype=list(
                    MetaData = matrix(ncol=0,nrow=0),
                    .MetaDataFx = list(),
                    Plots = list()
                  ),
                  validity = validPMCollection
)
