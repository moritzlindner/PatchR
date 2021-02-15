#' PatchMasteR: An environment for handling and analzing Patch Clamp measuremnts recorded with HEKA Patchmaster
#'
#' Core component of PatchMasteR is the Class PMRecording, wich Stores a Series imported from a Patchmaster *.dat file. Several PMRecordings can be assembled into a PMCollection.
#'
#' @section Import and creation functions:
#' * \link[=ImportPMRecording]{ImportPMRecording}
#' * \link[=addTrace]{addTrace}
#' * \link[=addPMCollection]{addPMCollection}
#' * \link[=newPMCollection]{newPMCollection}
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
#' * \link[=InspectSeries]{InspectSeries} (\link[=PMRecording]{PMRecording} objects)
#' * visualize Recparams/outliers
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
#' @keywords internal
"_PACKAGE"
