#' Makes PMRecording object scales convenient.
#'
#' This function converts scaling of a \link[=PMRecording]{PMRecording} or a \link[=PMCollection]{PMCollection} object and adjusts/adds SI prefixes
#'
#' @param object a \link[=PMRecording]{PMRecording} object
#' @return a \link[=PMRecording]{PMRecording} object
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
  min(
    decimals[
      decimals>=median(
        as.numeric(
          str_sub(
            format(X,scientific=T),
            (str_locate(format(X,scientific=T),
                        "e[\\+\\-]"))
            [,2]
          )
        )
      )
    ]
  )
}

convenientScalesvalue<-function(X){
  X/10^(convenientScalesdecimals(X))
}
#' @importFrom sitools f2si
convenientScalessi<-function(X){
  gsub("[^a-zA-Z]","",f2si(10^convenientScalesdecimals(X)))
}

setMethod("convenientScales",
          "PMRecording",
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
          "PMCollection",
          function(object){
            lapply(object,convenientScales,ReturnPMObject=T)
          }
)
