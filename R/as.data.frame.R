#' Converts PRecording or PCollection into a long data frame
#'
#' Converts PRecording or PCollection into a data frame in a long representation, analog to tidyR's gather
#'
#' @param x a \link[=PRecording]{PRecording} or \link[=PCollection]{PCollection}object
#' @exportMethod as.data.frame
setMethod("as.data.frame",
          "PRecording",
          function(x,
                   ...){
            Traces<-rep(getTraceNames(x),
                        each=(length(getSweepNames(x))*length(getTimeTrace(x))))
            sweeps<-type.convert(rep(rep(getSweepNames(x),
                                         each=length(getTimeTrace(x))),times=length(getTraceNames(x))))
            sweeps<-ordered(sweeps,levels=levels(getSweepNames(x)))
            times<-rep(getTimeTrace(x),
                       times=(length(getSweepNames(x))*length(getTraceNames(x))))

            out<-data.frame("Traces"=Traces,"Sweeps"=sweeps,"Times"=times)
            values<-NULL
            for (i in getTraceNames(x)){
              values<-c(values,unlist(apply(x@Data[[i]],2,function(x)x)))
            }
            cbind(out,values)
          }
)

#' @importFrom plyr ldply
#' @exportMethod as.data.frame
setMethod("as.data.frame",
          "PCollection",
          function(x,
                   ...){
            lst<-ldply(x@Series,as.data.frame)
            nms<-rep(x@Names,each=dim(as.data.frame(x@Series[[1]]))[1])
            grp<-rep(x@Group,each=dim(as.data.frame(x@Series[[1]]))[1])

            cbind(grp,nms,lst)
          }
)
