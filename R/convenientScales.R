
setGeneric(name="convenientScales",
           def=function(object)
           {
             standardGeneric("convenientScales")
           }
)

#' Makes PMRecording object scales convenient.
#'
#' This function converts scaling of a \link[=PMRecording]{PMRecording} or a \link[=PMCollection]{PMCollection} object and adjusts/adds SI prefixes
#'
#' @param object a \link[=PMRecording]{PMRecording} object
#' @return a \link[=PMRecording]{PMRecording} object
#' @exportMethod convenientScales
setMethod("convenientScales",
          "PMRecording",
          function(object){
            for (i in 1:length(TraceNames(object))){
              decimals<-seq(0,21,3)
              leading0<-median(attr(regexpr("(?<=\\.)0+", object@Data[[TraceNames(object)[i]]], perl = TRUE), "match.length")) # gives average no of leading zeros
              leading0<-min(decimals[decimals>leading0])
              object@Units[[i]]<-gsub("[^a-zA-Z]", "", sitools::f2si(10^-(leading0),object@Units[[i]]))
              object@Data[[TraceNames(object)[i]]]<-object@Data[[TraceNames(object)[i]]]*10^(leading0)
              print(dim(object@Data[[TraceNames(object)[i]]]))
            }
            object
          }
)

#' @exportMethod convenientScales
setMethod("convenientScales",
          "PMCollection",
          function(object){
            lapply(object,convenientScales)
          }
)
