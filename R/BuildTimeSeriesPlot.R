
setGeneric(name="BuildTimeSeriesPlot",
           def=function(object)
           {
             standardGeneric("BuildTimeSeriesPlot")
           }
)


#'#' Basic ggplots for time series data
#'
#' This function creats a basic visualization for each channel in the object. These will be stored in the Plots slot
#'
#' @param object a PMTrace object
#' @import ggplot2
#' @exportMethod BuildTimeSeriesPlot
setMethod("BuildTimeSeriesPlot",
          "PMTrace",
          function(object){
            tmp.object<-downsample(object,npnts.out = 1000)
            tmp.object<-convenientScales(tmp.object)
            dat<-toLong(tmp.object)
            for (i in tmp.object@Channels)
            {
              object@Plots[[i]]<-ggplot(dat[dat$Channels==i,])+
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
                ylab(paste(i,"[",tmp.object@Units[object@Channels==i],"]"))
            }
            return(object)
          }
)
