#' IV Protocol data
#'
#' This data set contains data from three traces from an IV recording protocol.
#'
#' @docType data
#'
#' @name .SampleData
#'
#' @usage data(PRecording)
#'
#' @format An object of class \code{"PRecording"}; see \link[=PRecording]{PRecording}.
#'
#' @keywords datasets
#'
#' @examples
#' rec<-Load(data(PRecording))
#' rec<-BuildRecordingPlot(rec)
#' Inspect(rec, what = "I-mon")
"PRecording"
