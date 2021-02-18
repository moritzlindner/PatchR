#' Basic ggplots forseries data
#'
#' This function creats a basic visualization for each Trace in the object. These will be stored in the Plots slot
#'
#' @param object a \link[=PRecording]{PRecording} object object
#' @return A \link[=PRecording]{PRecording} object
#' @exportMethod BuildSeriesPlot
setGeneric(name="BuildSeriesPlot",
           def=function(object)
           {
             standardGeneric("BuildSeriesPlot")
           }
)

setMethod("BuildSeriesPlot",
          "PRecording",
          function(object){
            tmp.object<-downsample(object,npnts.out = 1000)
            tmp.object<-convenientScales(tmp.object)
            dat<-as.data.frame(tmp.object)
            return(dat)
            for (i in tmp.object@Traces)
            {
              object@Plots[[i]]<-ggplot(dat[dat$Traces==i,])+
                geom_line(aes(y=values,x=Times,colour=as.numeric(Sweeps),group=Sweeps))+
                theme_classic()+
                theme(legend.position = "none",
                      text = element_text(size=8),
                      strip.background = element_rect(fill = "light grey",colour = NULL, size=0))+
                scale_colour_gradient2(
                  low = "blue",
                  mid = "yellow",
                  high = "red",
                  midpoint=mean(as.numeric(dat$Sweeps)))+
                xlab(paste("Time [",tmp.object@TimeUnit,"]"))+
                ylab(paste(i,"[",tmp.object@Units[object@Traces==i],"]"))
            }
            return(object)
          }
)
