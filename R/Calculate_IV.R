setGeneric(name="Calculate_IV",
           def=function(object,
                        X_FROM,
                        X_TO,
                        ReturnPMTRace=T,
                        I_Trace="I-mon",
                        V_Trace="V-mon",
                        x_D_FROM=NA,
                        X_D_TO=NA)
           {
             standardGeneric("Calculate_IV")
           }
)

#' Averaging PMSeries objects
#'
#' This function averages PMSeries objects by Trace, Sweep or Time
#'
#' @param object a PMSeries object
#' @param X_FROM,X_TO,x_D_FROM,X_D_TO Time points to perform averaging for IV prodcution
#' @param I_Trace,V_Trace Name of the traces containig I and V
#' @param ReturnPMTRace whether to return results as a PMSeries with an additional, computed trace
#' @return a matrix or PMSeries with IV data and ggplot stored in the MetaData and Plot slot, resp.
#' @exportMethod Calculate_IV
setMethod("Calculate_IV",
          "PMSeries",
          function(object,
                   X_FROM,
                   X_TO,
                   ReturnPMTRace=T,
                   I_Trace="I-mon",
                   V_Trace="V-mon",
                   x_D_FROM=NA,
                   X_D_TO=NA){
            out<-SubsetData(object, Time=c(X_FROM,X_TO))
            out<-apply(object,1,mean)

            if(!is.na(X_D_TO)){
              substract<-SubsetData(object, Time=c(x_D_FROM,X_D_TO))
              substract<-apply(object,1,mean)
              out[,"I.Substracted"]<-out[,I_Trace]-substract[,I_Trace]
            }
            if(ReturnPMTRace){
              object@MetaData[["IV"]]<-out

              object@Plot[["IV"]]<-ggplot(out)+
                geom_line(aes_string(y=I_Trace,x=V_Trace))+
                theme_classic()+
                theme(legend.position = "none",
                      text = element_text(size=8))+
                xlab(paste("Voltage [",object@Unit[getTraces(object)==V_Trace],"]"))+
                ylab(paste(i,"[",object@Unit[getTraces(object)==I_Trace],"]"))

              if(!is.na(X_D_TO)){
                object@Plot[["IV"]]<-ggplot(out)+
                  geom_line(aes_string(y="I.Substracted",x=V_Trace))+
                  theme_classic()+
                  theme(legend.position = "none",
                        text = element_text(size=8))+
                  xlab(paste("Voltage [",object@Unit[getTraces(object)==V_Trace],"]"))+
                  ylab(paste(i,"[",object@Unit[getTraces(object)=="I.Substracted"],"]"))
              }

              out<-object
            }
            out
          }
)
