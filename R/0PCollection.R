validPCollection<-function(object) {
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
  if (!all(unlist(lapply(object@Series,function(x) getTraceNames(x@RecordingParams))) %in% getTraceNames(object@RecordingParams))){
    ret<-ret+1
    print(getTraceNames(object@RecordingParams))
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


#' S4 class storing a collection of ePhys Treaces
#'
#' This class stores a collection of \linkS4class{PRecording}s in a single object. Facilitates identical processing of related of recordings.
#'
#' \describe{
#'    \item{Series}{A list containing individual PRecording objects of identical dimensisons, ProtocolName, RecMode and trace names}
#'    \item{Names}{Name identifier for the individual Series. Unique.}
#'    \item{Group}{Group identifier for the individual Series.}
#'    \item{MetaData}{A matrix with metatdata, e.g. calculated summay stats of each PRecording object. One row per entry in Series allowed}
#'    \item{Plots}{A slot to store ggplots}
#'    \item{RecordingParams}{Stores the Recording parameters that must be identical for all entries in Series (ProtocolName, RecMode, Traces)}
#'  }
#' @seealso \linkS4class{PRecording}
#' @exportClass PCollection
PCollection<-setClass(Class="PCollection",
                  slots =  list(Series="list",
                                Names="character",
                                Group="factor",
                                MetaData="matrix",
                                .MetaDataFx="list",
                                Plots="list",
                                RecordingParams="PRecordingParams"),
                  prototype=list(
                    MetaData = matrix(ncol=0,nrow=0),
                    .MetaDataFx = list(),
                    Plots = list()
                  ),
                  validity = validPCollection
)
