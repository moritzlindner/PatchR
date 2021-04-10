#' Converts PRecording or PCollection into a long data frame
#'
#' Converts \linkS4class[=PRecording]{PRecording} or \linkS4class[=PCollection]{PCollection}  into a data frame in a long representation, analog to tidyR's gather or
#'
#' @param x a \var{PRecording} or \var{PCollection} object.
#' @return A \var{data.frame}.
#' @seealso \link[base::as.data.frame]{as.data.frame()},\link[tidyr::gather]{gather()},\link[tidyr::pivot_longer]{pivot_longer()}
#' @exportMethod as.data.frame
setMethod("as.data.frame",
          "PRecording",
          function(x,
                   ...){
            Traces<-rep(GetTraceNames(x),
                        each=(length(GetSweepNames(x))*length(GetTimeTrace(x))))
            sweeps<-type.convert(rep(rep(GetSweepNames(x),
                                         each=length(GetTimeTrace(x))),times=length(GetTraceNames(x))))
            sweeps<-ordered(sweeps,levels=levels(GetSweepNames(x)))
            times<-rep(GetTimeTrace(x),
                       times=(length(GetSweepNames(x))*length(GetTraceNames(x))))

            out<-data.frame("Traces"=Traces,"Sweeps"=sweeps,"Times"=times)
            values<-NULL
            for (i in GetTraceNames(x)){
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
