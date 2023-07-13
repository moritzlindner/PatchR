#' Clear specified metadata columns from a PRecording or PCollection object
#'
#' Clears the specified columns from the MetaData slot of a \code{PRecording} object.
#'
#' @describeIn ClearMetaData This method clears the specified columns from the MetaData slot of a \code{PRecording} object.
#' @inheritParams Get
#' @param columns A character vector specifying the columns to clear. Defaults to all columns in the MetaData slot.
#' @param Verbose Logical indicating whether to display warnings for non-existing columns. Defaults to \code{TRUE}.
#' 
#' @return The modified \linkS4class{PRecording} or \linkS4class{PCollection} object with the specified columns cleared from the MetaData slot.
#' 
#' @details This method clears the specified columns from the MetaData slot of a \linkS4class{PRecording} or \linkS4class{PCollection} object. If all columns are cleared,
#' the MetaData slot and the associated \code{.MetaDataFx} slot are reset to empty values. If only specific columns are cleared,
#' the corresponding columns in the MetaData slot are removed.
#'
#' @examples
#' # Create a PRecording object with MetaData
#' data("PRecording")
#' # add two columns of metadata
#' SampleData<-AddMetaData(SampleData,1:length(GetSweepNames(SampleData)),"ID1")
#' SampleData<-AddMetaData(SampleData,length(GetSweepNames(SampleData)):1,"ID2")
#' SampleData<-AddMetaData(SampleData,length(GetSweepNames(SampleData)):1,"ID3")
#' SampleData
#' 
#' # Clear the "ID1" and "ID2" columns from the MetaData slot
#' cleared_SampleData <- ClearMetaData(SampleData, columns = c("ID1", "ID2"))
#' cleared_SampleData
#' 
#' # Clear all columns from the MetaData slot
#' cleared_SampleData2 <- ClearMetaData(SampleData)
#' cleared_SampleData2
#'
#' @seealso \linkS4class{PRecording} \linkS4class{PCollection}  \link{AddMetaData}
#'
#' @exportMethod ClearMetaData
setGeneric(
  name = "ClearMetaData",
  def = function(X,
                 columns = try(colnames(GetMetaData(X)), silent = T),
                 Verbose = T)
  {
    standardGeneric("ClearMetaData")
  }
)

#' @noMd
setMethod("ClearMetaData",
          "PRecording",
          function(X,
                   columns = try(colnames(GetMetaData(X)), silent = T)
                   ,
                   Verbose = T)
          {
            
            ec <- F
            ec <- tryCatch({
              GetMetaData(X)
              ec <- F
            }, error = function(e) {
              if (Verbose) {
                warning("Nothing to clear. MetaData slot is empty.")
              }
              ec <- T
              return(ec)
            })
            if (ec) { # if no MetaData stored, then return object directly 
              return(X)
            } else {
              if (!(any(columns %in% colnames(GetMetaData(X))))) {
                stop("Columns specified do not exist in MetaData slot")
              }
              
              if (Verbose) {
                if (!(all(columns %in% colnames(GetMetaData(X))))) {
                  warning("Not all columns specified exist in MetaData slot")
                  columns <-
                    columns[columns %in% colnames(GetMetaData(X))]
                }
              }
              
              if (all(colnames(GetMetaData(X)) %in% columns)) {
                X@MetaData = matrix(ncol = 0, nrow = 0)
                X@.MetaDataFx = list()
              } else{
                X@MetaData[, columns] <- NULL
                # FIXME: case where only one column left
                # FIXME: Name .MetaDataFx correspondingly to allow removal here
              }
              
              validPRecording(X)
              return(X)
            }
          })
#' @noMd
setMethod("ClearMetaData",
          "PCollection",
          function(X,
                   columns =  try(colnames(GetMetaData(X)), silent = T),
                   Verbose = T) {
            
            ec <- F
            ec <- tryCatch({
              GetMetaData(X)
              ec <- F
            }, error = function(e) {
              if (Verbose) {
                warning("Nothing to clear. MetaData slot is empty.")
              }
              ec <- T
              return(ec)
            })
            if (ec) { # if no MetaData stored, then return object directly 
              return(X)
            } else {
              if (!(any(columns %in% colnames(GetMetaData(X))))) {
                stop("Columns specified do not exist in MetaData Slot")
              }
              
              if (Verbose) {
                if (!(all(columns %in% colnames(GetMetaData(X))))) {
                  warning("Not all columns specified exist in MetaData Slot")
                  columns <-
                    columns[columns %in% colnames(GetMetaData(X))]
                }
              }
              
              if (all(colnames(GetMetaData(X)) %in% columns)) {
                X@MetaData = matrix(ncol = 0, nrow = 0)
                X@.MetaDataFx = list()
              } else{
                X@MetaData[, columns] <- NULL
                # FIXME: Name .MetaDataFx correspondingly to allow removal here
              }
              validPCollection(X)
              return(X)
            }
          })