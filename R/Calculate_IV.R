setGeneric(name="Calculate_IV",
           def=function(object,
                        X_FROM,
                        X_TO,
                        ReturnPMTRace=T,
                        ITrace="I-mon",
                        VTrace="V-mon",
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
#' @param ITrace,VTrace Name of the traces containig I and V
#' @param ReturnPMTRace whether to return results as a PMSeries with an additional, computed trace
#' @return a matrix or PMSeries with IV data and ggplot stored in the MetaData and Plot slot, resp.
#' @exportMethod Calculate_IV
setMethod("Calculate_IV",
          "PMSeries",
          function(object,
                   X_FROM,
                   X_TO,
                   ReturnPMTRace=T,
                   ITrace="I-mon",
                   VTrace="V-mon",
                   x_D_FROM=NA,
                   X_D_TO=NA){
            out<-SubsetData(object, Time=c(X_FROM,X_TO),nowarnings=T)
            out<-apply(object,1,mean)

            if(!is.na(X_D_TO)){
              substract<-SubsetData(object, Time=c(x_D_FROM,X_D_TO))
              substract<-apply(object,1,mean)
              out[,"I.Substracted"]<-out[,ITrace]-substract[,ITrace]
            }
            if(ReturnPMTRace){
             # object@MetaData[["IV"]]<-out
print(as.data.frame(out))
print(VTrace)
print(ITrace)
             # object@Plots[["IV"]]<-
                print(ggplot(as.data.frame(out))+
                geom_line(aes_string(y=ITrace,x=VTrace))+
                theme_classic())#+
                # theme(legend.position = "none",
                #       text = element_text(size=8))+
                # xlab(paste("Voltage [",object@Units[getTraces(object)==VTrace],"]"))+
                # ylab(paste("Current [",object@Units[getTraces(object)==ITrace],"]")))

              if(!is.na(X_D_TO)){
                object@Plots[["IV"]]<-ggplot(as.data.frame(out))+
                  geom_line(aes_string(y="I.Substracted",x=VTrace))+
                  theme_classic()+
                  theme(legend.position = "none",
                        text = element_text(size=8))+
                  xlab(paste("Voltage [",object@Units[getTraces(object)==VTrace],"]"))+
                  ylab(paste("Current [",object@Units[getTraces(object)=="I.Substracted"],"]"))
              }

              out<-object
            }
            out
          }
)
