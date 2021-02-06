#' Converts PMSeries x into a long data frame
#'
#' Converts PMSeries x into a data frame in a long representation, analog to tidyR's gather
#'
#' @param x a \link[=PMSeries]{PMSeries} object
#' @exportMethod as.data.frame
setMethod("as.data.frame",
          "PMSeries",
          function(x,
                   ...){
            Traces<-rep(getTraces(x),
                          each=(length(getSweeps(x))*length(getTimeTrace(x))))
            sweeps<-type.convert(rep(rep(getSweeps(x),
                                         each=length(getTimeTrace(x))),times=length(getTraces(x))))
            sweeps<-ordered(sweeps,levels=levels(getSweeps(x)))
            times<-rep(getTimeTrace(x),
                       times=(length(getSweeps(x))*length(getTraces(x))))

            out<-data.frame("Traces"=Traces,"Sweeps"=sweeps,"Times"=times)
            values<-NULL
            for (i in getTraces(x)){
              values<-c(values,unlist(apply(x@Data[[i]],2,function(x)x)))
            }
            cbind(out,values)
          }
)
