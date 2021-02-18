#' PatchMasteR: An environment for handling and analzing Patch Clamp measuremnts recorded with HEKA Patchmaster
#'
#' Core component of PatchMasteR is the PMRecording class, wich stores a series imported from a Patchmaster *.dat file. Several integrety checks are implemented into the PMRecording class to assure data integrity is maintained.
#' Multiple recordings (PMRecording) objects can be stored and processed within the PMCollection class.
#'
#' @import ggplot2 plotly sitools
#'
#'
#' @section Import and creation functions:
#' * \link[=ImportPMRecording]{ImportPMRecording} (for \link[=PMRecording]{PMRecording} objects) \cr
#' * \link[=addTrace]{addTrace} (for \link[=PMRecording]{PMRecording} objects) \cr
#' * \link[=newPMCollection]{newPMCollection} (for \link[=PMCollection]{PMCollection} objects) \cr
#' * \link[=addPMRecording]{addPMRecording} (for \link[=PMCollection]{PMCollection} objects) \cr
#' * \link[=dropPMRecording]{addPMRecording} (for \link[=PMCollection]{PMCollection} objects) \cr
#'
#' @section Accession functions for \link[=PMRecording]{PMRecording} objects:
#' * \link[=getCSlow]{getCSlow} \cr
#' * \link[=getSweepNames]{getSweepNames} \cr
#' * \link[=getTimeTrace]{getTimeTrace} \cr
#' * \link[=getTraceNames]{getTraceNames} \cr
#' * \link[=getMetaData]{getMetaData} \cr
#' * \link[=getRecParam]{getRecParam} \cr
#' * \link[=as.data.frame]{as.data.frame} \cr
#'
#' @section Accession functions for \link[=PMCollection]{PMCollection} objects:
#' * \link[=as.data.frame]{as.data.frame} \cr
#' * \link[=getMetaData]{getMetaData} \cr
#' * \link[=getRecParam]{getRecParam} \cr
#'
#' @section Visuaization functions:
#' * \link[=BuildSeriesPlot]{BuildSeriesPlot} (for \link[=PMRecording]{PMRecording} objects) \cr
#' * \link[=BuildQCPlot]{BuildSeriesPlot} (for \link[=PMCollection]{PMCollection} objects) \cr
#' * \link[=Plot_Inspect]{Plot_Inspect} (for \link[=PMRecording]{PMRecording} and \link[=PMCollection]{PMCollection} oojects) \cr
#' *visualize time series
#'
#' @section Math, conversion and subsampling functions:
#' * \link[=CurrentDensity]{CurrentDensity} (for \link[=PMRecording]{PMRecording} objects) \cr
#' * \link[=SubsetData]{SubsetData} \cr
#' * \link[=CalculateIV]{CalculateIV} CHECK!!! \cr
#' * \link[=apply]{apply} for \link[=PMRecording]{PMRecording} and \link[=lapply]{lapply} for \link[=PMCollection]{PMCollection} \cr
#'
#' @author \link[https://www.uni-marburg.de/en/fb20/departments/physiology/research/dominik-oliver-lab/research2/retinal-physiology-and-gene-therapy]{Moritz Lindner}
#'
#' @docType package
#' @name PatchMasteR
NULL
#> NULL
