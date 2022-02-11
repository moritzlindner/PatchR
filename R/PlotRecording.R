#' @describeIn Plot This function creates a basic visualization for each Trace in a single \linkS4class{PRecording} object.
#' @exportMethod PlotRecording
setGeneric(name="PlotRecording",
           def=function(X)
           {
             standardGeneric("PlotRecording")
           }
)

#' @describeIn Plot Method for PRecording
setMethod("PlotRecording",
          "PRecording",
          function(X){
            tmp.X<-downsample(X,npnts.out = 1000)
            tmp.X<-ConvenientScales(tmp.X)
            dat<-as.data.frame(tmp.X)
            dat$Sweeps<-as.numeric(dat$Sweeps)
            return(dat)
            for (i in tmp.X@Traces)
            {
              X@Plots[[i]]<-ggplot2::ggplot(dat[dat$Traces==i,])+
                ggplot2::geom_line(ggplot2::aes_string(y="values",x="Times",colour="Sweeps",group="Sweeps"))+
                ggplot2::theme_classic()+
                ggplot2::theme(legend.position = "none",
                      text = element_text(size=8),
                      strip.background = element_rect(fill = "light grey",colour = NULL, size=0))+
                ggplot2::scale_colour_gradient2(
                  low = "blue",
                  mid = "yellow",
                  high = "red",
                  midpoint=mean(as.numeric(dat$Sweeps)))+
                ggplot2::xlab(paste("Time [",tmp.X@TimeUnit,"]"))+
                ggplot2::ylab(paste(i,"[",tmp.X@Units[X@Traces==i],"]"))
            }
            return(X)
          }
)
