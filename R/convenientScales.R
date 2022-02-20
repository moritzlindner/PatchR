#' (OK) Makes PRecording object scales convenient.
#'
#' `r lifecycle::badge("stable")` \cr
#' This function converts scaling of a \linkS4class{PRecording} or \linkS4class{PCollection} object and adds SI prefixes. Caution: Currently not check is performed if Unit carries already a SI prefix.
#'
#' @inheritParams Get
#' @return A \linkS4class{PRecording} or \linkS4class{PCollection} object
#' @seealso \link[sitools:f2si]{sitools::f2si()}
#' @name ConvenientScales
#' @exportMethod ConvenientScales
setGeneric(name="ConvenientScales",
           def=function(X)
           {
             standardGeneric("ConvenientScales")
           }
)

#' @importFrom stringr str_sub str_locate
#' @importFrom stats median
ConvenientScalesdecimals<-function(X){
  decimals<-seq(-21,21,3)
  decimals[
    which.min(
      abs(
        decimals-stats::median(
          as.numeric(
            stringr::str_sub(
              format(X,scientific=T),
              (stringr::str_locate(format(X,scientific=T),
                          "e[\\+\\-]"))
              [,2]
            )
          )
        )
        )
      )
    ]
}

ConvenientScalesvalue<-function(X){
  X/10^(ConvenientScalesdecimals(X))
}
#' @importFrom sitools f2si
ConvenientScalessi<-function(X){
  gsub("[^a-zA-Z]","",sitools::f2si(10^ConvenientScalesdecimals(X)))
}


setMethod("ConvenientScales",
          "PRecording",
          function(X){
            for (i in 1:length(GetTraceNames(X))){
              X@Units[[i]]<-gsub("[^a-zA-Z]", "", sitools::f2si(10^ConvenientScalesdecimals(X@Data[[GetTraceNames(X)[i]]]),X@Units[[i]]))
              X@Data[[GetTraceNames(X)[i]]]<-ConvenientScalesvalue(X@Data[[GetTraceNames(X)[i]]])
            }
            X
          }
)

setMethod("ConvenientScales",
          "PCollection",
          function(X){
            lapply(X,ConvenientScales,ReturnPMobject=T)
          }
)
