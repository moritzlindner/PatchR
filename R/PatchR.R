#' Package documentation for PatchR
#'
#' Core component of PatchR is the PRecording class, wich stores a series imported from a Patchmaster *.dat file. Several integrety checks are implemented into the PRecording class to assure data integrity is maintained. \cr
#' Multiple recordings (PRecording) objects can be stored and processed within the PCollection class using mostly identical commands and synthax. \cr
#' Import procedures are currently implemented for HEKA's PachtMaster file format. \cr
#'
#' @import ggplot2
#'
#' @section Import and creation functions:
#' * \link[=ImportPRecording]{ImportPRecording} (for \link[=PRecording]{PRecording} objects) \cr
#' * \link[=AddTrace]{AddTrace} (for \link[=PRecording]{PRecording} objects) \cr
#' * \link[=newPCollection]{newPCollection} (for \link[=PCollection]{PCollection} objects) \cr
#' * \link[=AddPRecording]{AddPRecording} (for \link[=PCollection]{PCollection} objects) \cr
#' * \link[=DropPRecording]{AddPRecording} (for \link[=PCollection]{PCollection} objects) \cr
#'
#' @section Accession functions:
#' * \link[=GetCSlow]{GetCSlow} \cr
#' * \link[=GetSweepNames]{GetSweepNames} \cr
#' * \link[=GetTimeTrace]{GetTimeTrace} \cr
#' * \link[=GetTraceNames]{GetTraceNames} \cr
#' * \link[=GetMetaData]{GetMetaData} \cr
#' * \link[=GetRecParam]{GetRecParam} \cr
#' * \link[=as.data.frame]{as.data.frame} \cr
#'
#' @section Visuaization functions:
#' * \link[=PlotDoseResp]{PlotDoseResp} \cr
#' * \link[=PlotGroupComparison]{PlotGroupComparison} (for \link[=PCollection]{PCollection} objects) \cr
#' * \link[=PlotQC]{PlotRecording} (for \link[=PCollection]{PCollection} objects) \cr
#' * \link[=PlotRecording]{PlotRecording} (for \link[=PRecording]{PRecording} objects) \cr
#' * \link[=Plot_Inspect]{Plot_Inspect} (for \link[=PRecording]{PRecording} and \link[=PCollection]{PCollection} oojects) \cr
#' *visualize time series
#'
#' @section Math, conversion and subsampling functions:
#' * \link[=CurrentDensity]{CurrentDensity} (for \link[=PRecording]{PRecording} objects) \cr
#' * \link[=GetData]{GetData} \cr
#' * \link[=CalculateIV]{CalculateIV} - Outdated \cr
#' * \link[=apply]{apply} for \link[=PRecording]{PRecording} and \link[=lapply]{lapply} for \link[=PCollection]{PCollection} \cr
#'
#' @examples
#' \donttest{

#' # import a PatchMaster file
#' tmp<-ImportPRecording("test.dat",series = 1,traces = c(1,2))
#'
#' # build a Plot superimposing all sweeps and inspect interactivley
#' tmp<-PlotRecording(tmp)
#' tmp<-InspectSeries(tmp, Trace = "I.mon")
#'
#' # apply any function to the PRecording object, in this case, make mean over all sweeps
#' tmp<-apply(tmp, "Sweep", mean, ReturnPMObject = T)
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
#'                             getData(x, # extract values from I-mon trace, Sweep 15, between 1 and 1.19 s only
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
#' @author \link[https://www.uni-marburg.de/en/fb20/departments/physiology/research/dominik-oliver-lab/research2/retinal-physiology-and-gene-therapy]{Moritz Lindner}
#'
#' @docType package
#' @name PatchR
NULL
#> NULL
