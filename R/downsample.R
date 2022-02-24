#' Downsample a PRecording object
#'
#' `r lifecycle::badge("stable")` \cr
#' Downsample \link[=PRecording]{PRecording} object along the time axis
#'
#' @inheritParams Get
#' @param npnts.out Number of points in output time axis, ignored if NULL
#' @param sample.rate New sampling rate
#' @return A matrix or \link[=PRecording]{PRecording} object
#' @name Downsample
#' @exportMethod Downsample
setGeneric(name="Downsample",
           def=function(X,
                        npnts.out=NULL,
                        sample.rate=NULL)
           {
             standardGeneric("Downsample")
           }
)

#' @noMd
setMethod("Downsample",
          "PRecording",
          function(X,
                   npnts.out=NULL,
                   sample.rate=NULL){
            if(!is.null(npnts.out) && !is.null(sample.rate) ){
              stop("Cannnot subset by npnts.out and sample.rate")
            }
            if(!is.null(npnts.out)){
              if(round(length(GetTimeTrace(X))/npnts.out)!=length(GetTimeTrace(X))/npnts.out){
                warning("No of Sample points is not a multiple of npnts.out.")
              }
              keep<-GetTimeTrace(X)[seq(1,
                                             length(GetTimeTrace(X)),
                                             round(length(GetTimeTrace(X))/npnts.out))]
              X<-GetData(X,Time=keep,TimeExclusive=T)
            }
            if(!is.null(sample.rate)){
              donwsample.factor<-round(length(GetTimeTrace(X))/max(GetTimeTrace(X)))/sample.rate # current sample rate devided by target sample rate
              keep<-GetTimeTrace(X)[seq(1,
                                             length(GetTimeTrace(X)),
                                             donwsample.factor)]
              X<-GetData(X,Time=keep,TimeExclusive=T)
            }
            X
          }
)

#' @noMd
downsample <- Downsample
