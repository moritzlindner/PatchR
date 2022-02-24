#' DEPRECATED
#'
#' `r lifecycle::badge("deprecated")` \cr
#' This function is deprecated. Please use \link[=BuildStimRespPlot]{BuildStimRespPlot} instead. \cr This function averages \link[=PRecording]{PRecording} objects by Trace, Sweep or Time. If X is a \link[=PCollection]{PCollection}. then does so for each recording stored in the object.
#'
#' @param X a \link[=PRecording]{PRecording} object
#' @param X_FROM,X_TO,x_D_FROM,X_D_TO Time points to perform averaging for IV prodcution
#' @param ITrace,VTrace Name of the traces/channels containig Current(I) and and Voltage(V)
#' @param ReturnPObject whether to return results as a \link[=PRecording]{PRecording}  with an additional, computed trace. If set to \code{FALSE}, will return a \link[base:matrix]{base::matrix}. Default is \code{TRUE}.
#' @return a matrix or PRecording with IV \link[base:matrix]{base::matrix} and \link[ggplot2:ggplot]{ggplot2::ggplot} stored in the MetaData and Plot slot, resp.
#' @name Calculate_IV
#' @importFrom lifecycle deprecate_warn
#' @importFrom ggplot2 ggplot geom_line aes_string theme_classic theme element_text xlab ylab
#' @exportMethod Calculate_IV
setGeneric(name="Calculate_IV",
           def=function(X,
                        X_FROM,
                        X_TO,
                        ReturnPObject=T,
                        ITrace="I-mon",
                        VTrace="V-mon",
                        x_D_FROM=NA,
                        X_D_TO=NA)
           {
             standardGeneric("Calculate_IV")
           }
)

setMethod("Calculate_IV",
          "PRecording",
          function(X,
                   X_FROM,
                   X_TO,
                   ReturnPObject=T,
                   ITrace="I-mon",
                   VTrace="V-mon",
                   x_D_FROM=NA,
                   X_D_TO=NA){
            deprecate_warn("0.9.0", "PatchR::Calculate_IV()", "BuildStimRespPlot()")
            out<-GetData(X, Time=c(X_FROM,X_TO),nowarnings=T)
            out<-ConvenientScales(out)
            out<-apply(X,1,mean)

            if(!is.na(X_D_TO)){
              substract<-GetData(X, Time=c(x_D_FROM,X_D_TO))
              substract<-apply(X,1,mean)
              out[,"I.Substracted"]<-out[,ITrace]-substract[,ITrace]
            }
            if(ReturnPObject){
              X<-AddMetaData(X,out)

              out<-as.data.frame(out)
              rownames(out)<-NULL
              colnames(out)[colnames(out)==VTrace]<-"Voltage"
              colnames(out)[colnames(out)==ITrace]<-"Current"
              X@Plots[["IV"]]<-ggplot(out)+
                geom_line(aes_string(y="Current",x="Voltage"))+
                theme_classic()+
                theme(legend.position = "none",
                      text = element_text(size=8))+
                xlab(paste("Voltage [",X@Units[GetTraceNames(X)==VTrace],"]"))+
                ylab(paste("Current [",X@Units[GetTraceNames(X)==ITrace],"]"))

              if(!is.na(X_D_TO)){
                X@Plots[["IV.Substracted"]]<-ggplot(as.data.frame(out))+
                  geom_line(aes_string(y="I.Substracted",x="Voltage"))+
                  theme_classic()+
                  theme(legend.position = "none",
                        text = element_text(size=8))+
                  xlab(paste("Voltage [",X@Units[GetTraceNames(X)==VTrace],"]"))+
                  ylab(paste("Current [",X@Units[GetTraceNames(X)=="I.Substracted"],"]"))
              }

              out<-X
            }
            out
          }
)

setMethod("Calculate_IV",
          "PCollection",
          function(X,
                   X_FROM,
                   X_TO,
                   ReturnPObject=T,
                   ITrace="I-mon",
                   VTrace="V-mon",
                   x_D_FROM=NA,
                   X_D_TO=NA){
            lapply(X,function(x){Calculate_IV(X,X_FROM,X_TO,ReturnPObject,ITrace="I-mon",VTrace="V-mon",x_D_FROM,X_D_TO)})
          }
)
