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
  if (!all(unlist(lapply(object@Series,function(x) GetTraceNames(x@RecordingParams))) %in% GetTraceNames(object@RecordingParams))){
    ret<-ret+1
    print(GetTraceNames(object@RecordingParams))
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
#' @slot Series A list containing individual \linkS4class{PRecording} objects of identical dimensions, \var{ProtocolName}, \var{RecMode} and \var{TraceNames}.
#' @slot Names A \var{vector} of type \code{character} as unique identifier for the individual PRecording objects stored in the collection.
#' @slot Group A \var{vector} of type \code{logical} as Group identifier for the individual Series.
#' @slot Metadata A \var{matrix} with each row corresponding to a \linkS4class{PRecording} stored in the \linkS4class{PCollection}. Column names must be unique.
#' @slot Plots A slot to store ggplots
#' @slot .MetaDataFx A list fo functions called to under \link[=AddMetaData]{AddMetaData()}
#' @slot RecordingParams Stores the Recording parameters that must be identical for all entries in Series \var{ProtocolName}, \var{RecMode} and \var{TraceNames}.
#' @seealso \linkS4class{PRecording}
#' @include PatchR.R 0PRecordingParams.R
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
