#' PatchMasteR: An environment for handling and analzing Patch Clamp measuremnts recorded with HEKA Patchmaster
#'
#' Core component of PatchMasteR is the Class PMTrace, wich Stores a Series imported from a Patchmaster *.dat file. Several PMTraces can be assembled into a PMExperiment.
#'
#' @section Import functions:
#' * \link[=ImportPMSeries]{ImportPMSeries}
#' * \link[=addTrace]{addTrace}
#'
#' @section Accession functions for \link[=PMSeries]{PMSeries} objects:
#' * \link[=getCs]{getCs}
#' * \link[=getSweeps]{getSweeps}
#' * \link[=getTimeTrace]{getTimeTrace}
#' * \link[=getTraces]{getTraces}
#' * \link[=as.data.frame]{as.data.frame}
#'
#' @section Accession functions for \link[=PMExperiment]{PMExperiment} objects:
#' * \link[=as.data.frame]{as.data.frame}
#'
#' @section Visuaization functions
#' * \link[=BuildSeriesPlot]{BuildSeriesPlot} (\link[=PMSeries]{PMSeries} objects)
#' * \link[=InspectTimeSeries]{InspectTimeSeries} (\link[=PMSeries]{PMSeries} objects)
#'
#' @section Math, conversion and subsampling functions:
#' * \link[=CurrentDensity]{CurrentDensity} (only for for \link[=PMSeries]{PMSeries} objects)
#' * \link[=SubsetData]{SubsetData}
#' * \link[=CalculateIV]{CalculateIV}
#' * \link[=apply]{apply} for for \link[=PMSeries]{PMSeries} and \link[=lapply]{lapply} for for \link[=PMExperiment]{PMExperiment}
#'
#' @docType package
#' @name PatchMasteR
#' @keywords internal
"_PACKAGE"
