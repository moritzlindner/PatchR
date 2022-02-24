#' Load HDF5 files containing PRecording or PCollection
#'
#' `r lifecycle::badge("stable")` \cr
#' This function loads HDF5 files containing \linkS4class{PRecording} or \linkS4class{PCollection} data.

#' @param filename Path to the file to be loaded.
#' @return A \linkS4class{PRecording} or \linkS4class{PCollection} object.
#' @importFrom hdf5r h5attr H5File
#' @importFrom utils compareVersion packageDescription
#' @examples
#' fn<-tempfile()
#' Save(SampleData,fn)
#' object.size(SampleData)
#' rm(SampleData)
#' SampleData<-Load(fn)
#' object.size(SampleData)
#' @seealso \link[=Save]{Save}, \link[=PRecording]{PRecording}, \link[=PCollection]{PCollection} objects
#' @export
Load <- function(filename) {
  file <- H5File$new(filename, mode = "r")
  message(paste("Loading", file$attr_open("Type")$read(), "from", filename))
  message(
    paste(
      "File was created on",
      file$attr_open("Created")$read(),
      "with PatchR",
      file$attr_open("Version")$read()
    )
  )
  if (compareVersion(file$attr_open("Version")$read(),
                     packageDescription("PatchR")$Version) < 0) {
    warning("File was created with newer version of PatchR")
  }

  if (file$attr_open("Type")$read() == "PRecording") {
    # if is a PRecording
    X <- Load.Recording(file)
  }

  if (file$attr_open("Type")$read() == "PCollection") {
    # if is a PCollection

    Recordings <- file$open("Recordings")
    SER_LIST <- list()
    for (i in Recordings$names) {
      message("Loading recording ", as.character(as.numeric(i)), " of ",as.character(max(as.numeric(Recordings$names))))
      SER_LIST[as.numeric(i)] <- Load.Recording(Recordings$open(i))
    }
    params <- Load.RecordingParams(file$open("RecordingParams"))

    X <- PCollection(
      Recordings = SER_LIST,
      Names = file$open("Names")$read(),
      Group = as.factor(file$open("Group")$read()),
      RecordingParams = params
    )
    X <- Load.Metadata(file, X)
  }
  file$close_all()
  X
}

Load.RecordingParams <- function(RecordingParams) {
  PRecordingParams(
    Traces = RecordingParams$open("Traces")$read(),
    RecMode = RecordingParams$open("RecMode")$read(),
    ProtocolName = RecordingParams$open("ProtocolName")$read(),
    RPip = RecordingParams$open("RPip")$read(),
    RSeal = RecordingParams$open("RSeal")$read(),
    URest = RecordingParams$open("URest")$read(),
    Cs = RecordingParams$open("Cs")$read(),
    Rs = RecordingParams$open("Rs")$read(),
    Experiment = if (RecordingParams$open("Experiment")$dims > 0) {
      RecordingParams$open("Experiment")$read()
    } else{
      ""
    },
    Series =  if (RecordingParams$open("Series")$dims > 0) {
      RecordingParams$open("Series")$read()
    } else{
      ""
    },
    Type = if (RecordingParams$open("Type")$dims > 0) {
      RecordingParams$open("Type")$read()
    } else{
      ""
    },
    Version = if (RecordingParams$open("Version")$dims > 0) {
      RecordingParams$open("Version")$read()
    } else{
      ""
    },
    Created = as.POSIXct(RecordingParams$open("Created")$read(), origin = "1970-01-01"),
    Filename = if (RecordingParams$open("Filename")$dims > 0) {
      RecordingParams$open("Filename")$read()
    } else{
      ""
    }
  )
}

Load.Recording <- function(con) {
  params <- Load.RecordingParams(con$open("RecordingParams"))
  dat <- con$open("Data")
  Data <- list()
  for (i in dat$names) {
    Data[[i]] <- dat$open(i)$read()
    colnames(Data[[i]]) <- dat$open(i)$attr_open("colnames")$read()
  }
  X<-PRecording(
    Traces = con$open("Traces")$read(),
    Units = con$open("Units")$read(),
    TimeTrace = con$open("TimeTrace")$read(),
    TimeUnit = con$open("TimeUnit")$read(),
    Sweeps = ordered(con$open("Sweeps")$read(), levels = con$open("Sweeps")$read()),
    SweepTimes = con$open("SweepTimes")$read(),
    Data = Data,
    Plots = list(),
    RecordingParams = params
  )
  X <- Load.Metadata(con, X)
  X
}

Load.Metadata <- function(con, Pobject) {
  if ("MetaData" %in% con$names){
    Pobject@MetaData <- as.matrix(con$open("MetaData")$read())
    colnames(Pobject@MetaData) <-
      con$open("MetaData")$attr_open("colnames")$read()
    MetaDataFx <- con$open(".MetaDataFx")
    Pobject@.MetaDataFx <- list()
    for (i in MetaDataFx$names) {
      Pobject@.MetaDataFx[[as.numeric(i)]]<-parse(text =MetaDataFx$open(i)$read())
    }
  }
  Pobject
}
