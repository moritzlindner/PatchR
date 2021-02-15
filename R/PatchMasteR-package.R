#' PatchMasteR: An environment for handling and analzing Patch Clamp measuremnts recorded with HEKA Patchmaster
#'
#' Core component of PatchMasteR is the PMRecording class, wich stores a series imported from a Patchmaster *.dat file. Several integrety checks are implemented into the PMRecording class to assure data integrity is maintained.
#' Multiple recordings (PMRecording) objects can be stored and processed within the PMCollection class.
#'
#' @section Import and creation functions:
#' * \link[=ImportPMRecording]{ImportPMRecording} (\link[=PMRecording]{PMRecording} objects)
#' * \link[=addTrace]{addTrace} (\link[=PMRecording]{PMRecording} objects)
#' * \link[=newPMCollection]{newPMCollection} (\link[=PMCollection]{PMCollection} objects)
#' * \link[=addPMRecording]{addPMRecording} (\link[=PMCollection]{PMCollection} objects)
#' * \link[=dropPMRecording]{addPMRecording} (\link[=PMCollection]{PMCollection} objects)
#'
#' @section Accession functions for \link[=PMRecording]{PMRecording} objects:
#' * \link[=getCs]{getCs}
#' * \link[=SweepNames]{SweepNames}
#' * \link[=getTimeTrace]{getTimeTrace}
#' * \link[=TraceNames]{TraceNames}
#' * \link[=getMetaData]{getMetaData}
#' * \link[=RecParam]{RecParam}
#' * \link[=as.data.frame]{as.data.frame}
#'
#' @section Accession functions for \link[=PMCollection]{PMCollection} objects:
#' * \link[=as.data.frame]{as.data.frame}
#' * \link[=getMetaData]{getMetaData}
#' * \link[=RecParam]{RecParam}
#'
#' @section Visuaization functions:
#' * \link[=BuildSeriesPlot]{BuildSeriesPlot} (\link[=PMRecording]{PMRecording} objects)
#' * \link[=BuildQCPlot]{BuildSeriesPlot} (\link[=PMCollection]{PMCollection} objects)
#' * \link[=Inspect]{Inspect} (\link[=PMRecording]{PMRecording} and \link[=PMCollection]{PMCollection} oojects)
#' *visualize time series
#'
#' @section Math, conversion and subsampling functions:
#' * \link[=CurrentDensity]{CurrentDensity} (only for for \link[=PMRecording]{PMRecording} objects)
#' * \link[=SubsetData]{SubsetData}
#' * \link[=CalculateIV]{CalculateIV} CHECK!!!
#' * \link[=apply]{apply} for for \link[=PMRecording]{PMRecording} and \link[=lapply]{lapply} for for \link[=PMCollection]{PMCollection}
#'
#' @docType package
#' @name PatchMasteR
NULL
#> NULL
