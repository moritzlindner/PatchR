#' PatchR: An environment for handling and analyzing electrophysiological (Patch Clamp) measurements
#'
#' Core component of PatchR is the PRecording class, wich stores a series imported from a Patchmaster *.dat file. Several integrety checks are implemented into the PRecording class to assure data integrity is maintained.
#' Multiple recordings (PRecording) objects can be stored and processed within the PCollection class using mostly identical commands and synthax.
#'
#' @import ggplot2 plotly sitools
#'
#'
#' @section Import and creation functions:
#' * \link[=ImportPRecording]{ImportPRecording} (for \link[=PRecording]{PRecording} objects) \cr
#' * \link[=addTrace]{addTrace} (for \link[=PRecording]{PRecording} objects) \cr
#' * \link[=newPCollection]{newPCollection} (for \link[=PCollection]{PCollection} objects) \cr
#' * \link[=addPRecording]{addPRecording} (for \link[=PCollection]{PCollection} objects) \cr
#' * \link[=dropPRecording]{addPRecording} (for \link[=PCollection]{PCollection} objects) \cr
#'
#' @section Accession functions for \link[=PRecording]{PRecording} objects:
#' * \link[=getCSlow]{getCSlow} \cr
#' * \link[=getSweepNames]{getSweepNames} \cr
#' * \link[=getTimeTrace]{getTimeTrace} \cr
#' * \link[=getTraceNames]{getTraceNames} \cr
#' * \link[=getMetaData]{getMetaData} \cr
#' * \link[=getRecParam]{getRecParam} \cr
#' * \link[=as.data.frame]{as.data.frame} \cr
#'
#' @section Accession functions for \link[=PCollection]{PCollection} objects:
#' * \link[=as.data.frame]{as.data.frame} \cr
#' * \link[=getMetaData]{getMetaData} \cr
#' * \link[=getRecParam]{getRecParam} \cr
#'
#' @section Visuaization functions:
#' * \link[=PlotRecording]{PlotRecording} (for \link[=PRecording]{PRecording} objects) \cr
#' * \link[=PlotQC]{PlotRecording} (for \link[=PCollection]{PCollection} objects) \cr
#' * \link[=Plot_Inspect]{Plot_Inspect} (for \link[=PRecording]{PRecording} and \link[=PCollection]{PCollection} oojects) \cr
#' *visualize time series
#'
#' @section Math, conversion and subsampling functions:
#' * \link[=CurrentDensity]{CurrentDensity} (for \link[=PRecording]{PRecording} objects) \cr
#' * \link[=SubsetData]{SubsetData} \cr
#' * \link[=CalculateIV]{CalculateIV} CHECK!!! \cr
#' * \link[=apply]{apply} for \link[=PRecording]{PRecording} and \link[=lapply]{lapply} for \link[=PCollection]{PCollection} \cr
#'
#' @author \link[https://www.uni-marburg.de/en/fb20/departments/physiology/research/dominik-oliver-lab/research2/retinal-physiology-and-gene-therapy]{Moritz Lindner}
#'
#' @docType package
#' @name PatchR
NULL
#> NULL
