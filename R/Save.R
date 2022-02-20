#' (OK) Saves PRecording or PCollection objects to HDF5 files
#'
#' `r lifecycle::badge("stable")` \cr
#'This function saves \linkS4class{PCollection} or \linkS4class{PCollection} objects into HDF5 files.
#' @param X A \linkS4class{PCollection} or \linkS4class{PCollection} object.
#' @param filename Path to the file data should be written to.
#' @param overwrite Should existing files be overwritten?
#' @return Does not return any values.
#' @exportMethod Save
setGeneric(
  name = "Save",
  def = function(X,
                 filename,
                 overwrite = F)
  {
    standardGeneric("Save")
  }
)

#' @importFrom hdf5r H5File
setMethod("Save",
          "PRecording",
          function(X,
                   filename,
                   overwrite) {
            if (file.exists(filename) & overwrite == F) {
              stop(paste("File", filename, "already exists."))
            } else{
              if (file.exists(filename)) {
                warning(paste("File", filename, "already exists. Overwriting."))
              }
              file <-
                Save.Recording(hdf5r::H5File$new(filename, mode = "w"), X)
              file$create_attr("Type", "PRecording")
              file$create_attr("Created", format(Sys.time()))
              file$create_attr("Version", packageDescription("PatchR")$Version)
              file$create_attr("Platform", version$platform)
              file$close_all()
            }
          })

#' @importFrom hdf5r H5File
setMethod("Save",
          "PCollection",
          function(X,
                   filename,
                   overwrite) {
            if (file.exists(filename) & overwrite == F) {
              stop(paste("File", filename, "already exists."))
            } else{
              if (file.exists(filename)) {
                warning(paste("File", filename, "already exists. Overwriting."))
              }
              file <- hdf5r::H5File$new(filename, mode = "w")
              tryCatch({
                message("Writing recording parameters...")
                RecordingParams <-
                  Save.RecordingParams(file$create_group("RecordingParams"),
                                       X@RecordingParams)
                message("Writing collection info...")
                file[["Names"]] <- X@Names
                file[["Group"]] <- as.character(X@Group)
                file<-Save.Metadata(file, X)
                # write each Series
                SER_SLOT <- file$create_group("Series")
                for (i in 1:length(X)) {
                  message(paste("Writing series data", i, "-", names(X)[i]))
                  suppressWarnings(Series <-
                                     Save.Recording(
                                       SER_SLOT$create_group(as.character(i)),
                                       GetData(X, Series = i)
                                     ))
                }
                message("Writing timestamp and version info...")
                file$create_attr("Type", "PCollection")
                file$create_attr("Created", format(Sys.time()))
                file$create_attr("Version", packageDescription("PatchR")$Version)
                file$create_attr("Platform", version$platform)
                file$close_all()
                message("Complete.")
              },
              error = function(e) {
                file$create_attr("Type", "Incomplete PCollection")
                file$create_attr("Created", format(Sys.time()))
                file$create_attr("Version", packageDescription("PatchR")$Version)
                file$create_attr("Platform", version$platform)
                file$close_all()
                stop(paste("An error occured saving", filename),
                     ". Writing aborted. Closing file...")
              })

            }
          })

#' @importFrom hdf5r h5types H5S
#' @importFrom methods slot
Save.RecordingParams <- function (con, PRecordingParams) {
  for (j in slotNames(PRecordingParams)) {
    warning(j)
    if (j %in% c("Created", "Experiment", "ProtocolName", "FileName")) {
      con$create_dataset(name=j,
                         robj=as.character(methods::slot(PRecordingParams, j)),
                         dtype = hdf5r::h5types$char,
                         space = hdf5r::H5S$new(
                           "simple",
                           dims = length(methods::slot(PRecordingParams, j)),
                           maxdims = Inf),
                         chunk_dims = 1024
      )
    } else{
      con[[j]] <- slot(PRecordingParams, j)
    }
  }
  con
}

#' @importFrom hdf5r h5attr
Save.Metadata <- function(con,Pobject){
  if (max(dim(Pobject@MetaData))>0){
    con$create_dataset(name = "MetaData",Pobject@MetaData,gzip_level = 9)
    hdf5r::h5attr(con[["MetaData"]], "colnames") <- as.character(colnames(Pobject@MetaData))
    hdf5r::h5attr(con[["MetaData"]], "rownames") <- as.character(rownames(Pobject@MetaData))

    FXs <- con$create_group(".MetaDataFx")
    MetaDataFx<-lapply(collection@.MetaDataFx,as.character)
    for (i in 1:length(MetaDataFx)){
      FXs[[as.character(i)]]<-MetaDataFx[[i]]
    }
  }
  con
}

#' @importFrom hdf5r h5attr
Save.Recording <- function (con, PRecording) {
  con[["Traces"]] <- PRecording@Traces
  con[["Units"]] <- PRecording@Units
  con[["TimeTrace"]] <- PRecording@TimeTrace
  con[["TimeUnit"]] <- PRecording@TimeUnit
  con[["Sweeps"]] <- as.character(PRecording@Sweeps)
  con[["SweepTimes"]] <- PRecording@SweepTimes
  Data <- con$create_group("Data")
  for (i in names(PRecording@Data)) {
    Data$create_dataset(i,PRecording@Data[[i]],gzip_level = 9)
    #Data[[i]] <- PRecording@Data[[i]]
    hdf5r::h5attr(Data[[i]], "colnames") <- colnames(PRecording@Data[[i]])
  }

  con<-Save.Metadata(con, PRecording)

  RecordingParams <-
    Save.RecordingParams(con$create_group("RecordingParams"),
                         PRecording@RecordingParams)

  con
}
