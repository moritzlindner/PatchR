#' PatchMasteR: An environment for handling and analzing Patch Clamp measuremnts recorded with HEKA Patchmaster
#'
#' Core component of PatchMasteR is the Class PMTrace, wich Stores a Series imported from a Patchmaster *.dat file
#'
#' @section Import functions:
#' \link[=ImportPMSeries]{ImportPMSeries}
#' \link[=addTrace]{addTrace}
#'
#' @section Accession functions:
#' \link[=getCs]{getCs}
#' \link[=getSweeps]{getSweeps}
#' \link[=getTimeTrace]{getTimeTrace}
#' \link[=getTraces]{getTraces}
#' \link[=as.data.frame]{as.data.frame}
#'
#' @section Visuaization functions:
#' \link[=BuildTimeSeriesPlot]{BuildTimeSeriesPlot}
#' \link[=InspectTimeSeries]{InspectTimeSeries}
#'
#' @section Math, conversion and subsampling functions:
#' \link[=CurrentDensity]{CurrentDensity}
#' \link[=SubsetData]{SubsetData}
#' \link[=CalculateIV]{CalculateIV}
#' \link[=apply]{apply}
#'
#' @docType package
#' @name PatchMasteR
#' @keywords internal
"_PACKAGE"
