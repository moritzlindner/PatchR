#' PatchMasteR: An environment for handling and analzing Patch Clamp measuremnts recorded with HEKA Patchmaster
#'
#' Core component of PatchMasteR is the Class PMTrace, wich Stores a Series imported from a Patchmaster *.dat file
#'
#' @section Import functions:
#' ImportPMSeries
#'
#' @section Accession functions:
#' getCs
#' getSweeps
#' getTimeTrace
#' getTraces
#' as.data.frame
#'
#' @section Visuaization functions:
#' BuildTimeSeriesPlot
#' InspectTimeSeries
#'
#' @section Math, conversion and subsampling functions:
#' CurrentDensity
#' SubsetData
#' CalculateIV
#' apply
#'
#'
#' @docType package
#' @name PatchMasteR
NULL
