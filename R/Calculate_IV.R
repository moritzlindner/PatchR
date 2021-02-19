#' OUTDATED. Caluclates IV from a PRecording or PCollection object
#'
#' This function averages \link[=PRecording]{PRecording} objects by Trace, Sweep or Time. If object is a \link[=PCollection]{PCollection}. then does so for each Series stored in the object
#'
#' @param object a \link[=PRecording]{PRecording} object
#' @param X_FROM,X_TO,x_D_FROM,X_D_TO Time points to perform averaging for IV prodcution
#' @param ITrace,VTrace Name of the traces containig Current(I) and and Voltage(V)
#' @param ReturnPMObject whether to return results as a \link[=PRecording]{PRecording}  with an additional, computed trace. If set to \code{FALSE}, will return a \link[=base::matrix]{base::matrix}. Default is \code{TRUE}.
#' @return a matrix or PRecording with IV \link[=base::matrix]{base::matrix} and \link[=ggplot2::ggplot]{ggplot2::ggplot} stored in the MetaData and Plot slot, resp.
NULL

setGeneric(name="Calculate_IV",
           def=function(object,
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
setMethod("Calculate_IV",
          "PRecording",
          function(object,
                   X_FROM,
                   X_TO,
                   ReturnPMObject=T,
                   ITrace="I-mon",
                   VTrace="V-mon",
                   x_D_FROM=NA,
                   X_D_TO=NA){
            out<-getData(object, Time=c(X_FROM,X_TO),nowarnings=T)
            out<-convenientScales(out)
            out<-apply(object,1,mean)

            if(!is.na(X_D_TO)){
              substract<-getData(object, Time=c(x_D_FROM,X_D_TO))
              substract<-apply(object,1,mean)
              out[,"I.Substracted"]<-out[,ITrace]-substract[,ITrace]
            }
            if(ReturnPMObject){
              object<-AddMetaData(object,out)

              out<-as.data.frame(out)
              rownames(out)<-NULL
              colnames(out)[colnames(out)==VTrace]<-"Voltage"
              colnames(out)[colnames(out)==ITrace]<-"Current"
              object@Plots[["IV"]]<-ggplot(out)+
                geom_line(aes(y=Current,x=Voltage))+
                theme_classic()+
                theme(legend.position = "none",
                      text = element_text(size=8))+
                xlab(paste("Voltage [",object@Units[getTraceNames(object)==VTrace],"]"))+
                ylab(paste("Current [",object@Units[getTraceNames(object)==ITrace],"]"))

              if(!is.na(X_D_TO)){
                object@Plots[["IV.Substracted"]]<-ggplot(as.data.frame(out))+
                  geom_line(aes_string(y=I.Substracted,x=Voltage))+
                  theme_classic()+
                  theme(legend.position = "none",
                        text = element_text(size=8))+
                  xlab(paste("Voltage [",object@Units[getTraceNames(object)==VTrace],"]"))+
                  ylab(paste("Current [",object@Units[getTraceNames(object)=="I.Substracted"],"]"))
              }

              out<-object
            }
            out
          }
)

#' @exportMethod Calculate_IV
setMethod("Calculate_IV",
          "PCollection",
          function(object,
                   X_FROM,
                   X_TO,
                   ReturnPMObject=T,
                   ITrace="I-mon",
                   VTrace="V-mon",
                   x_D_FROM=NA,
                   X_D_TO=NA){
            lapply(object,function(x){Calculate_IV(object,X_FROM,X_TO,ReturnPMObject,ITrace="I-mon",VTrace="V-mon",x_D_FROM,X_D_TO)})
          }
)
