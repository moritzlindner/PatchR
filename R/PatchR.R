#' PatchR: An environment for handling and analyzing electrophysiological (Patch Clamp) measurements
#'
#' Core component of PatchR is the PRecording class, wich stores a series imported from a Patchmaster *.dat file. Several integrety checks are implemented into the PRecording class to assure data integrity is maintained.
#' Multiple recordings (PRecording) objects can be stored and processed within the PCollection class using mostly identical commands and synthax.
#'
#' @import ggplot2 plotly
#'
#'
#' @section Import and creation functions:
#' * \link[=ImportPRecording]{ImportPRecording} (for \link[=PRecording]{PRecording} objects) \cr
#' * \link[=AddTrace]{AddTrace} (for \link[=PRecording]{PRecording} objects) \cr
#' * \link[=newPCollection]{newPCollection} (for \link[=PCollection]{PCollection} objects) \cr
#' * \link[=AddPRecording]{AddPRecording} (for \link[=PCollection]{PCollection} objects) \cr
#' * \link[=dropPRecording]{AddPRecording} (for \link[=PCollection]{PCollection} objects) \cr
#'
#' @section Accession functions for \link[=PRecording]{PRecording} objects:
#' * \link[=GetCSlow]{GetCSlow} \cr
#' * \link[=GetSweepNames]{GetSweepNames} \cr
#' * \link[=GetTimeTrace]{GetTimeTrace} \cr
#' * \link[=GetTraceNames]{GetTraceNames} \cr
#' * \link[=GetMetaData]{GetMetaData} \cr
#' * \link[=GetRecParam]{GetRecParam} \cr
#' * \link[=as.data.frame]{as.data.frame} \cr
#'
#' @section Accession functions for \link[=PCollection]{PCollection} objects:
#' * \link[=as.data.frame]{as.data.frame} \cr
#' * \link[=GetMetaData]{GetMetaData} \cr
#' * \link[=GetRecParam]{GetRecParam} \cr
#'
#' @section Visuaization functions:
#' * \link[=PlotRecording]{PlotRecording} (for \link[=PRecording]{PRecording} objects) \cr
#' * \link[=PlotQC]{PlotRecording} (for \link[=PCollection]{PCollection} objects) \cr
#' * \link[=Plot_Inspect]{Plot_Inspect} (for \link[=PRecording]{PRecording} and \link[=PCollection]{PCollection} oojects) \cr
#' *visualize time series
#'
#' @section Math, conversion and subsampling functions:
#' * \link[=CurrentDensity]{CurrentDensity} (for \link[=PRecording]{PRecording} objects) \cr
#' * \link[=GetData]{GetData} \cr
#' * \link[=CalculateIV]{CalculateIV} CHECK!!! \cr
#' * \link[=apply]{apply} for \link[=PRecording]{PRecording} and \link[=lapply]{lapply} for \link[=PCollection]{PCollection} \cr
#'
#' @author \link[https://www.uni-marburg.de/en/fb20/departments/physiology/research/dominik-oliver-lab/research2/retinal-physiology-and-gene-therapy]{Moritz Lindner}
#'
#' @docType package
#' @name 0_PatchR
NULL
#> NULL
