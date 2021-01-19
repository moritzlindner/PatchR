
setGeneric(name="toLong",
           def=function(object)
           {
             standardGeneric("toLong")
           }
)

#' toLong
#'
#' converts data from a PMSeries object into its long representation, analog to tidyR's gather
#'
#' @name toLong
#' @param object a PMSeries object
#' @exportMethod toLong
setMethod("toLong",
          "PMSeries",
          function(object){
            Traces<-rep(getTraces(object),
                          each=(length(getSweeps(object))*length(getTimeTrace(object))))
            sweeps<-type.convert(rep(rep(getSweeps(object),
                                         each=length(getTimeTrace(object))),times=length(getTraces(object))))
            times<-rep(getTimeTrace(object),
                       times=(length(getSweeps(object))*length(getTraces(object))))

            out<-data.frame("Traces"=Traces,"Sweeps"=sweeps,"Times"=times)
            values<-NULL
            for (i in getTraces(object)){
              values<-c(values,unlist(apply(object@Data[[i]],2,function(x)x)))
            }
            cbind(out,values)
          }
)
