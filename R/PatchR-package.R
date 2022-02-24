#' PatchR - An environment for working with electrophysiology recordings
#'
#' Core component of PatchR is the PRecording class, which stores a series imported from a Patchmaster .dat file or an Axon .abf file. Several checks are implemented into the PRecording class to assure data integrity is maintained. \cr
#' Multiple recordings \code{PRecording} objects containing identically shaped data (i.e. recorded witht the same protocol) can be stored and processed within the \code{PCollection} class using mostly identical commands and syntax. \cr
#' Import procedures are currently implemented for HEKA's PachtMaster .dat and Axon Instruments .abf file format. \cr
#'
## usethis namespace: start
#' @importFrom lifecycle badge deprecated
#' @import ggplot2
#'
#' @section File operations:
#' * \link[=ImportPRecording]{ImportPRecording} (for \link[=PRecording]{PRecording} objects) \cr
#' * \link[=AddTrace]{AddTrace} (for \link[=PRecording]{PRecording} objects) \cr
#' * \link[=NewPCollection]{newPCollection} (for \link[=PCollection]{PCollection} objects) \cr
#' * \link[=AddPRecording]{AddPRecording} (for \link[=PCollection]{PCollection} objects) \cr
#' * \link[=DropPRecording]{AddPRecording} (for \link[=PCollection]{PCollection} objects) \cr
#' * \link[=Load]{Load} (for \link[=PRecording]{PRecording} and \link[=PCollection]{PCollection} objects)  \cr
#' * \link[=Save]{Save} (for \link[=PRecording]{PRecording} and \link[=PCollection]{PCollection} objects)  \cr
#'
#' @section Accession functions:
#' * \link[=GetData]{GetData} \cr
#' * \link[=GetPlot]{GetPlot} \cr
#' * \link[=GetMetaData]{GetMetaData} \cr
#' * \link[=GetRecParam]{GetRecParam} \cr
#' * \link[=GetCSlow]{GetCSlow} \cr
#' * \link[=GetSweepNames]{GetSweepNames} \cr
#' * \link[=GetTimeTrace]{GetTimeTrace} \cr
#' * \link[=GetTraceNames]{GetTraceNames} \cr
#' * \link[=GetGroupMembers]{GetGroupMembers} \cr
#' * \link[=GetRecordingNames]{GetRecordingNames} \cr
#' * \link[=GetPlotNames]{GetPlotNames} \cr
#' * \link[=GetPlot]{GetPlot} \cr
#' * \href{../../PatchR/html/as.data.frame-PRecording-method.html}{as.data.frame()} \cr
#'
#' @section Measurement functions:
#' * \link[=Measure]{Measure} \cr
#' * \link[=MeasureStimResp]{MeasureStimResp} \cr
#' * \link[=MeasureSweeps]{MeasureSweeps} \cr
#'
#' @section Visuaization functions:
#' * \link[=BuildStimRespPlot]{BuildStimRespPlot} \cr
#' * \link[=BuildGroupComparisonPlot]{BuildGroupComparisonPlot} (for \link[=PCollection]{PCollection} objects) \cr
#' * \link[=BuildQCPlot]{BuildRecordingPlot} (for \link[=PCollection]{PCollection} objects) \cr
#' * \link[=BuildRecordingPlot]{BuildRecordingPlot} (for \link[=PRecording]{PRecording} objects) \cr
#' * \link[=Inspect]{Inspect} (for \link[=PRecording]{PRecording} and \link[=PCollection]{PCollection} objects) \cr
#'
#' @section Math, conversion and subsampling functions:
#' * \link[=CurrentDensity]{CurrentDensity} (for \link[=PRecording]{PRecording} objects) \cr
#' * \link[=GetData]{GetData} \cr
#' * \link[=Calculate_IV]{Calculate_IV} - Outdated \cr
#' * \href{../../PatchR/html/apply-PRecording-method.html}{apply()} for \link[=PRecording]{PRecording} and \href{../../PatchR/html/apply-PRecording-method.html}{lapply()} for \link[=PCollection]{PCollection} \cr
#'
#'
#' @examples
#' \dontrun{
#' # Example 1
#' # import a PatchMaster file
#' tmp<-ImportPRecording("test.dat",series = 1,traces = c(1,2))
#'
#' # build a Plot superimposing all sweeps and inspect interactivley
#' tmp<-BuildRecordingPlot(tmp)
#' tmp<-InspectSeries(tmp, Trace = "I.mon")
#'
#' # apply any function to the PRecording object, in this case, make mean over all sweeps
#' tmp<-apply(tmp, "Sweep", mean, ReturnPObject = TRUE)
#'
#' # and return as data.frame
#' as.data.frame(tmp)
#'
#' # Example 2
#'
#' exp<-newPCollection(list(tmp,tmp),Names=c("A","B"),Group=c("Generic1","Generic2"))
#' AddMetaData(exp, #add as metadata to PCollection
#'             lapply(exp, # for each PRecording stored in PCollection
#'                    function(x){
#'                       apply( # average over time
#'                             getData(x,
#'                             # extract values from I-mon trace,
#'                             # Sweep 15, between 1 and 1.19 s only
#'                                     Traces="I-mon",
#'                                     Time =c(1,1.19),
#'                                     Sweeps=getSweepNames(x)[c(15)]),
#'                             "Time",
#'                             mean)
#'                    }
#'             )
#' )
#'
#' exp@MetaData
#'
#' }
#'
#'
#' @author \href{https://www.lindnerlab.de}{Moritz Lindner}
#'
#' @docType package
#' @name .Overview
"_PACKAGE"
