#' @describeIn Plots This function creates a basic visualization of quality-relevant recording parameters in the PCollection
#' @exportMethod PlotQC
setGeneric(name="PlotQC",
           def=function(object)
           {
             standardGeneric("PlotQC")
           }
)

setMethod("PlotQC",
          "PCollection",
          function(object){
            items<-c("Cs", "Rs")

            dat<-cbind(object@Names,object@Group,as.data.frame(getRecParam(object,items)))
            colnames(dat)<-c("Name","Group",items)

            dat[,"Cs"]<-dat[,"Cs"]/sitools::pico
            dat[,"Rs"]<-dat[,"Rs"]/sitools::mega
            object@Plots[["QC_Metrics"]]<-ggplot(dat,aes(x=Cs,y=Rs,fill=Group,group=Name))+geom_point(position = "jitter")+
              theme_classic()+
              theme(legend.position = "none",
                    text = element_text(size=8),
                    strip.background = element_rect(fill = "light grey",colour = NULL, size=0))+
              xlab(paste("Cs [pF]"))+
              ylab(paste("Rs [MOhm]"))
            return(object)
          }
)
