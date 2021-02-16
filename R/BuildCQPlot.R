
setGeneric(name="BuildQCPlot",
           def=function(object)
           {
             standardGeneric("BuildQCPlot")
           }
)


#' Visualize quality metrics for PMRecordings in PMCollection
#'
#' This function creates a basic visualization of quality-relevant recording parameters in the PMCollection These will be stored in the Plots slot and can be accessed using \link[=Inspect]{Inspect}(object,"QC_Metrics")
#'
#' @param object a \link[=PMCollection]{PMCollection} object object
#' @return A \link[=PMCollection]{PMCollection} object
#' @exportMethod BuildQCPlot
setMethod("BuildQCPlot",
          "PMCollection",
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
