#' Makes PRecording object scales convenient.
#'
#' This function converts scaling of a \link[=PRecording]{PRecording} or a \link[=PCollection]{PCollection} object and adjusts/adds SI prefixes
#'
#' @param object a \link[=PRecording]{PRecording} object
#' @return a \link[=PRecording]{PRecording} object
#' @exportMethod convenientScales
setGeneric(name="convenientScales",
           def=function(object)
           {
             standardGeneric("convenientScales")
           }
)

#' @importFrom stringr str_sub str_locate
convenientScalesdecimals<-function(X){
  decimals<-seq(-21,21,3)
  decimals[
    which.min(
      abs(
        decimals-median(
          as.numeric(
            str_sub(
              format(X,scientific=T),
              (str_locate(format(X,scientific=T),
                          "e[\\+\\-]"))
              [,2]
            )
          )
        )
        )
      )
    ]
}

convenientScalesvalue<-function(X){
  X/10^(convenientScalesdecimals(X))
}
#' @importFrom sitools f2si
convenientScalessi<-function(X){
  gsub("[^a-zA-Z]","",f2si(10^convenientScalesdecimals(X)))
}

setMethod("convenientScales",
          "PRecording",
          function(object){
            for (i in 1:length(getTraceNames(object))){
              decimals<-seq(0,21,3)
              object@Units[[i]]<-gsub("[^a-zA-Z]", "", si(10^-convenientScalesdecimals(X),object@Units[[i]]))
              object@Data[[getTraceNames(object)[i]]]<-convenientScalesvalue(object@Data[[getTraceNames(object)[i]]])
            }
            object
          }
)
setMethod("convenientScales",
          "PCollection",
          function(object){
            lapply(object,convenientScales,ReturnPMObject=T)
          }
)
