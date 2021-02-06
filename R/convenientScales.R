
setGeneric(name="convenientScales",
           def=function(object)
           {
             standardGeneric("convenientScales")
           }
)

#' Makes PMSeries object scales convenient.
#'
#' This function converts scaling of a \link[=PMSeries]{PMSeries} object and adjusts/adds SI prefixes
#'
#' @param object a \link[=PMSeries]{PMSeries} object
#' @return a \link[=PMSeries]{PMSeries} object
#' @exportMethod convenientScales
setMethod("convenientScales",
          "PMSeries",
          function(object){
            for (i in 1:length(getTraces(object))){
              decimals<-seq(0,21,3)
              leading0<-median(attr(regexpr("(?<=\\.)0+", object@Data[[getTraces(object)[i]]], perl = TRUE), "match.length")) # gives average no of leading zeros
              leading0<-min(decimals[decimals>leading0])
              object@Units[[i]]<-gsub("[^a-zA-Z]", "", sitools::f2si(10^-(leading0),object@Units[[i]]))
              object@Data[[getTraces(object)[i]]]<-object@Data[[getTraces(object)[i]]]*10^(leading0)
              print(dim(object@Data[[getTraces(object)[i]]]))
            }
            object
          }
)
