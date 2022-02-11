#' OUTDATED. Caluclates IV from a PRecording or PCollection object
#'
#' This function is outdated. Please use \link[=PlotStimResp]{PlotStimResp} instead. This function averages \link[=PRecording]{PRecording} objects by Trace, Sweep or Time. If X is a \link[=PCollection]{PCollection}. then does so for each Series stored in the X
#'
#' @param X a \link[=PRecording]{PRecording} object
#' @param X_FROM,X_TO,x_D_FROM,X_D_TO Time points to perform averaging for IV prodcution
#' @param ITrace,VTrace Name of the traces containig Current(I) and and Voltage(V)
#' @param ReturnPMObject whether to return results as a \link[=PRecording]{PRecording}  with an additional, computed trace. If set to \code{FALSE}, will return a \link[base:matrix]{base::matrix}. Default is \code{TRUE}.
#' @return a matrix or PRecording with IV \link[base:matrix]{base::matrix} and \link[ggplot2:ggplot]{ggplot2::ggplot} stored in the MetaData and Plot slot, resp.
#' @name Calculate_IV
#' @exportMethod Calculate_IV
setGeneric(name="Calculate_IV",
           def=function(X,
                        X_FROM,
                        X_TO,
                        ReturnPMObject=T,
                        ITrace="I-mon",
                        VTrace="V-mon",
                        x_D_FROM=NA,
                        X_D_TO=NA)
           {
             standardGeneric("Calculate_IV")
           }
)

#' @describeIn Calculate_IV Method for PRecording
setMethod("Calculate_IV",
          "PRecording",
          function(X,
                   X_FROM,
                   X_TO,
                   ReturnPMObject=T,
                   ITrace="I-mon",
                   VTrace="V-mon",
                   x_D_FROM=NA,
                   X_D_TO=NA){
            out<-GetData(X, Time=c(X_FROM,X_TO),nowarnings=T)
            out<-ConvenientScales(out)
            out<-apply(X,1,mean)

            if(!is.na(X_D_TO)){
              substract<-GetData(X, Time=c(x_D_FROM,X_D_TO))
              substract<-apply(X,1,mean)
              out[,"I.Substracted"]<-out[,ITrace]-substract[,ITrace]
            }
            if(ReturnPMObject){
              X<-AddMetaData(X,out)

              out<-as.data.frame(out)
              rownames(out)<-NULL
              colnames(out)[colnames(out)==VTrace]<-"Voltage"
              colnames(out)[colnames(out)==ITrace]<-"Current"
              X@Plots[["IV"]]<-ggplot2::ggplot(out)+
                ggplot2::geom_line(ggplot2::aes_string(y="Current",x="Voltage"))+
                ggplot2::theme_classic()+
                ggplot2::theme(legend.position = "none",
                      text = element_text(size=8))+
                ggplot2::xlab(paste("Voltage [",X@Units[GetTraceNames(X)==VTrace],"]"))+
                ggplot2::ylab(paste("Current [",X@Units[GetTraceNames(X)==ITrace],"]"))

              if(!is.na(X_D_TO)){
                X@Plots[["IV.Substracted"]]<-ggplot2::ggplot(as.data.frame(out))+
                  ggplot2::geom_line(ggplot2::aes_string(y=I.Substracted,x=Voltage))+
                  ggplot2::theme_classic()+
                  ggplot2::theme(legend.position = "none",
                        text = element_text(size=8))+
                  ggplot2::xlab(paste("Voltage [",X@Units[GetTraceNames(X)==VTrace],"]"))+
                  ggplot2::ylab(paste("Current [",X@Units[GetTraceNames(X)=="I.Substracted"],"]"))
              }

              out<-X
            }
            out
          }
)

#' @describeIn Calculate_IV Method for PCollection
setMethod("Calculate_IV",
          "PCollection",
          function(X,
                   X_FROM,
                   X_TO,
                   ReturnPMObject=T,
                   ITrace="I-mon",
                   VTrace="V-mon",
                   x_D_FROM=NA,
                   X_D_TO=NA){
            lapply(X,function(x){Calculate_IV(X,X_FROM,X_TO,ReturnPMObject,ITrace="I-mon",VTrace="V-mon",x_D_FROM,X_D_TO)})
          }
)
