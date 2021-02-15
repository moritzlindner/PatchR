
setGeneric(name="BuildQCPlot",
           def=function(object)
           {
             standardGeneric("BuildQCPlot")
           }
)


#' Visualize quality metrics for PMRecordings in PMCollection
#'
#' This function creates a basic visualization of quality-relevant recording parameters in the PMCollection These will be stored in the Plots slot
#'
#' @param object a \link[=PMCollection]{PMCollection} object object
#' @return A \link[=PMCollection]{PMCollection} object
#' @import ggplot2 plotly
#' @exportMethod BuildQCPlot
setMethod("BuildQCPlot",
          "PMRecording",
          function(object){

            dat<-cbind(object@Name,object@Group,RecParam(object,c("Cs", "Rs")))
            colnames(dat)<-c("Name","Group","Cs","Rs")
            object@Plots[["QC_Metrics"]]<-ggplot(dat,aes(x=CS,y=Rs,fill=Group))+geom_point()+
              theme_classic()+
              theme(legend.position = "none",
                    text = element_text(size=8),
                    strip.background = element_rect(fill = "light grey",colour = NULL, size=0))+
              xlab(paste("Cs [F]"))+
              ylab(paste("Rs [Ohm]"))

            ggplotly(object@Plots[["QC_Metrics"]])

            return(object)
          }
)
