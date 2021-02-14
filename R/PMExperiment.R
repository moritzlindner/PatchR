validPMExperiment<-function(object) {
  ret=0
  if (!(length(object@Series) == length(object@Names))){
    ret+1
    print("Name list incompatible to items in Series")
  }
  if(any(duplicated(object@Names))){
    ret+1
    cat("Name provided not unique: ", object@Names[duplicated(object@Names)])
  }
  if (!(length(object@Series) == length(object@Group))){
    ret+1
    print("Group list incompatible to items in Series")
  }
  if(!all(dim(object@MetaData)==0)){
    if (!(dim(object@MetaData)[1] == length(object@Group))){
      ret+1
      print("MetaData incompatible to items in Series")
    }
  }
  if (!all(unlist(lapply(object@Series,function(x) getTraces(x@RecordingParams))) %in% getTraces(object@RecordingParams))){
    ret+1
    print(getTraces(object@RecordingParams))
    print("Unequal trace names")
  }

  if (!all(lapply(object@Series,function(x) x@RecordingParams@ProtocolName)==object@RecordingParams@ProtocolName)){
    ret+1
    print("Unequal Protocol names")
  }

  if (!all(lapply(object@Series,function(x) x@RecordingParams@RecMode)==object@RecordingParams@RecMode)){
    ret+1
    print("Unequal Recording modes")
  }

  if(ret==0) {TRUE} else {FALSE}
}


#' S4 class storing imported [Patch Master](https://www.heka.com/downloads/downloads_main.html#down_patchmaster)  Traces.
#'
#' This class stores imported [Patch Master](https://www.heka.com/downloads/downloads_main.html#down_patchmaster)  Traces. Currently only checked for time series. It is strictly validated to assure data consistency
#'
#' \describe{
#'    \item{Series}{A list containing individual PMSeries objects of identical dimensisons, ProtocolName, RecMode and trace names}
#'    \item{Names}{Name identifier for the individual Series. Unique.}
#'    \item{Group}{Group identifier for the individual Series.}
#'    \item{MetaData}{A matrix with metatdata, e.g. calculated summay stats of each PMSeries object. One row per entry in Series allowed}
#'    \item{Plots}{A slot to store ggplots}
#'    \item{RecordingParams}{Stores the Recording parameters that must be identical for all entries in Series (ProtocolName, RecMode, Traces)}
#'  }
#' @exportClass PMExperiment
PMExperiment<-setClass(Class="PMExperiment",
                  slots =  list(Series="list",
                                Names="character",
                                Group="factor",
                                MetaData="matrix",
                                .MetaDataFx="list",
                                Plots="list",
                                RecordingParams="PMRecordingParams"),
                  validity = validPMExperiment
)
