

setGeneric(name="downsample",
           def=function(object,
                        npnts.out=NULL,
                        sample.rate=NULL)
           {
             standardGeneric("downsample")
           }
)
#' Donwsample a PRecording object
#'
#' Downsample \link[=PRecording]{PRecording} object along the time axis
#'
#' @param object a \link[=PRecording]{PRecording} object
#' @param npnts.out Number of points in output time axis, ignored if NULL
#' @param sample.rate New sampling rate in 1/[object@TimeUnit]
#' @return A matrix or \link[=PRecording]{PRecording} object
#' @exportMethod downsample
setMethod("downsample",
          "PRecording",
          function(object,
                   npnts.out=NULL,
                   sample.rate=NULL){
            if(!is.null(npnts.out) && !is.null(sample.rate) ){
              stop("Cannnot subset by npnts.out and sample.rate")
            }
            if(!is.null(npnts.out)){
              if(round(length(getTimeTrace(object))/npnts.out)!=length(getTimeTrace(object))/npnts.out){
                warning("No of Sample points is not a multiple of npnts.out.")
              }
              keep<-getTimeTrace(object)[seq(1,
                                             length(getTimeTrace(object)),
                                             round(length(getTimeTrace(object))/npnts.out))]
              object<-getData(object,Time=keep,TimeExclusive=T)
            }
            if(!is.null(sample.rate)){
              donwsample.factor<-round(length(getTimeTrace(object))/max(getTimeTrace(object)))/sample.rate # current sample rate devided by target sample rate
              keep<-getTimeTrace(object)[seq(1,
                                             length(getTimeTrace(object)),
                                             donwsample.factor)]
              object<-getData(object,Time=keep,TimeExclusive=T)
            }
            object
          }
)
