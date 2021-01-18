
setGeneric(name="toLong",
           def=function(object)
           {
             standardGeneric("toLong")
           }
)

#' toLong
#'
#' converts data from a PMTrace object into its long representation, analog to tidyR's gather
#'
#' @name toLong
#' @param object a PMTrace object
#' @exportMethod toLong
setMethod("toLong",
          "PMTrace",
          function(object){
            channels<-rep(getChannels(object),
                          each=(length(getSweeps(object))*length(getTimeTrace(object))))
            sweeps<-type.convert(rep(rep(getSweeps(object),
                                         each=length(getTimeTrace(object))),times=length(getChannels(object))))
            times<-rep(getTimeTrace(object),
                       times=(length(getSweeps(object))*length(getChannels(object))))

            out<-data.frame("Channels"=channels,"Sweeps"=sweeps,"Times"=times)
            values<-NULL
            for (i in getChannels(object)){
              values<-c(values,unlist(apply(object@Data[[i]],2,function(x)x)))
            }
            cbind(out,values)
          }
)
