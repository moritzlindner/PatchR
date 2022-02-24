#' Converts PRecording or PCollection into a long data frame
#'
#' `r lifecycle::badge("stable")` \cr
#' Converts \linkS4class{PRecording} or \linkS4class{PCollection} into a data frame in a long representation, analog to tidyR's \link[tidyr:gather]{gather()}/\link[tidyr:pivot_longer]{pivot_longer()}.
#'
#' @param x A \var{PRecording} or \var{PCollection} object.
#' @param row.names,optional,... Unused.
#' @return A \code{data.frame}.
#' @seealso \link[base:as.data.frame]{as.data.frame()}, \link[tidyr:gather]{gather()}, \link[tidyr:pivot_longer]{pivot_longer()}
#' @examples
#' data(PRecording)
#' # Subset SampleData to only keep three time points and convert into data.frame
#' as.data.frame(GetData(SampleData,Time = c(0.4,0.8,1.2),TimeExclusive = T))
#' @name as.data.frame
#' @exportMethod as.data.frame
NULL

#' @describeIn as.data.frame Method for PRecording
setMethod("as.data.frame",
          "PRecording",
          function(x,
                   ...){
            Traces<-rep(GetTraceNames(x),
                        each=(length(GetSweepNames(x))*length(GetTimeTrace(x))))
            sweeps<-utils::type.convert(rep(rep(GetSweepNames(x),
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
            lst<-ldply(x@Recordings,as.data.frame)
            nms<-rep(x@Names,each=dim(as.data.frame(x@Recordings[[1]]))[1])
            grp<-rep(x@Group,each=dim(as.data.frame(x@Recordings[[1]]))[1])

            cbind(grp,nms,lst)
          }
)
