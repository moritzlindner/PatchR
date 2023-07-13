#' Adds metadata to a PRecording or PCollection object
#'
#' `r lifecycle::badge("stable")` \cr
#' This function adds metadata to a \linkS4class{PRecording} or \linkS4class{PCollection} object by filling the \var{MetaData} slot. The function calling \code{AddMetaData} is stored in the \var{.MetaDataFx} slot.
#'
#' @inheritParams Get
#' @param values The values to be added. Can be anything convertible into a \var{matrix}.
#' @param title The title(s) for the metadata column(s).
#' @param Verbose should the names/titles of the columns added be printed?
#' @return A \linkS4class{PRecording} or \linkS4class{PCollection} object, respectively.
#' @details
#' \strong{The MetaData slot} \cr
#'  In a \var{PRecording} object the \var{MetaData} slot is a \var{matrix} with each row corresponding to a sweep. \cr
#'  In a \var{PCollection} object the \var{MetaData} slot is a \var{matrix} with each row corresponding to a \var{PRecording} stored in the object.
#'  Column names must be unique.
#' @seealso \linkS4class{PRecording}, \linkS4class{PCollection}, \href{../../PatchR/html/apply-PRecording-method.html}{apply()}, \href{../../PatchR/html/lapply-PCollection-method.html}{lapply()}, \link[base:as.matrix]{as.matrix()}
#' @examples
#' data("PRecording")
#' # add two columns of metadata
#' SampleData<-AddMetaData(SampleData,1:length(GetSweepNames(SampleData)),"ID1")
#' SampleData<-AddMetaData(SampleData,length(GetSweepNames(SampleData)):1,"ID2")
#' GetMetaData(SampleData,"ID1")
#' GetMetaData(SampleData)
#' @name AddMetaData
#' @exportMethod AddMetaData
setGeneric(
  name = "AddMetaData",
  def = function(X,
                 values,
                 title = colnames(values),
                 Verbose = T)
  {
    standardGeneric("AddMetaData")
  }
)

#' @noMd
setMethod("AddMetaData",
          "PRecording",
          function(X,
                   values,
                   title = colnames(values),
                   Verbose = T) {
            if (any(duplicated(title))) {
              stop("Duplicate MetaData names not allowed")
            }
            if(is.null(colnames(values))){
              if(is.null(title)){
                stop("Title(s) for the metadata columns must be provided if values has no column names.")
              }
            }
            
            if (any(title %in% colnames(X@MetaData))) {
              stop("MetaData names already in use. Use ClearMetaData(X,title) to remove.")
            }
            values <- as.matrix(values)
            colnames(values) <- title
            if (Verbose) {
              message("Adding metadata column(s) ", title)
            }
            if (all(dim(X@MetaData) == 0)) {
              X@MetaData <- values
              colnames(X@MetaData) <- as.vector(title)
              X@.MetaDataFx[[1]] <- sys.calls()[[1]]
            } else{
              cn<-colnames(X@MetaData)
              X@MetaData <- cbind(X@MetaData,values)
              colnames(X@MetaData) <- c(cn, title)
              X@.MetaDataFx <-
                append(X@.MetaDataFx, sys.calls()[[1]])
            }

            if (!validPRecording(X)) {
              stop(paste(
                "Adding Metadata to PRecording",
                deparse(substitute(X)),
                "failed. Incorrect dimensison"
              ))
            }
            X
          })

#' @noMd
setMethod("AddMetaData",
          "PCollection",
          function(X,
                   values,
                   title = colnames(values),
                   Verbose = T) {
            if (any(duplicated(title))) {
              stop("Duplicate MetaData names not allowed")
            }
            if (any(title %in% colnames(X@MetaData))) {
              stop("MetaData names already in use")
            }

            values <- as.matrix(values)
            colnames(values) <- title
            if (Verbose) {
              message("Adding metadata column(s) ", title)
            }
            if (all(dim(X@MetaData) == 0)) {
              X@MetaData <- values
              colnames(X@MetaData) <- as.vector(title)
              X@.MetaDataFx[[1]] <- sys.calls()[[1]]
            } else{
              cn<-colnames(X@MetaData)
              X@MetaData <- cbind(X@MetaData, values)
              colnames(X@MetaData) <- c(cn, title)
              X@.MetaDataFx <-
                append(X@.MetaDataFx, sys.calls()[[1]])
            }

            if (!validPCollection(X)) {
              stop(paste(
                "Adding Metadata to PCollection",
                deparse(substitute(X)),
                "failed. incorrect dimensison"
              ))
            }
            X
          })
