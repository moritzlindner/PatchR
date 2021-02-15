#' Converts PMRecording or PMCollection into a long data frame
#'
#' Converts PMRecording or PMCollection into a data frame in a long representation, analog to tidyR's gather
#'
#' @param x a \link[=PMRecording]{PMRecording} or \link[=PMCollection]{PMCollection}object
#' @exportMethod as.data.frame
setMethod("as.data.frame",
          "PMRecording",
          function(x,
                   ...){
            Traces<-rep(TraceNames(x),
                        each=(length(SweepNames(x))*length(getTimeTrace(x))))
            sweeps<-type.convert(rep(rep(SweepNames(x),
                                         each=length(getTimeTrace(x))),times=length(TraceNames(x))))
            sweeps<-ordered(sweeps,levels=levels(SweepNames(x)))
            times<-rep(getTimeTrace(x),
                       times=(length(SweepNames(x))*length(TraceNames(x))))

            out<-data.frame("Traces"=Traces,"Sweeps"=sweeps,"Times"=times)
            values<-NULL
            for (i in TraceNames(x)){
              values<-c(values,unlist(apply(x@Data[[i]],2,function(x)x)))
            }
            cbind(out,values)
          }
)

#' @import plyr
#' @exportMethod as.data.frame
setMethod("as.data.frame",
          "PMCollection",
          function(x,
                   ...){
            lst<-plyr::ldply(x@Series,as.data.frame)
            nms<-rep(x@Names,each=dim(as.data.frame(x@Series[[1]]))[1])
            grp<-rep(x@Group,each=dim(as.data.frame(x@Series[[1]]))[1])

            cbind(grp,nms,lst)
          }
)
