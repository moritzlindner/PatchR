#' @describeIn Plot This function creates a basic visualization of quality-relevant recording parameters in the PCollection
#' @exportMethod PlotQC
setGeneric(name="PlotQC",
           def=function(X)
           {
             standardGeneric("PlotQC")
           }
)

#' @describeIn Plot Method for PCollection
setMethod("PlotQC",
          "PCollection",
          function(X){
            items<-c("Cs", "Rs")

            dat<-cbind(X@Names,X@Group,as.data.frame(GetRecParam(X,items)))
            colnames(dat)<-c("Name","Group",items)

            dat[,"Cs"]<-dat[,"Cs"]/sitools::pico
            dat[,"Rs"]<-dat[,"Rs"]/sitools::mega
            X@Plots[["QC_Metrics"]]<-ggplot2::ggplot(dat,aes_string(x="Cs",y="Rs",fill="Group",group="Name"))+geom_point(position = "jitter")+
              ggplot2::theme_classic()+
              ggplot2::theme(legend.position = "none",
                    text = element_text(size=8),
                    strip.background = ggplot2::element_rect(fill = "light grey",colour = NULL, size=0))+
              ggplot2::xlab(paste("Cs [pF]"))+
              ggplot2::ylab(paste("Rs [MOhm]"))
            return(X)
          }
)
