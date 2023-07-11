#' Import electrophysiology recordings
#'
#' `r lifecycle::badge("stable")` \cr
#' This function imports electrophysiology recordings from PatchMaster *.dat files ("Series") or Axon's ABF files and creates a PRecording object. Support for WinWCP files is thought in future.
#'
#' @param filename Path to file. Currently only [Patch Master .dat](https://www.heka.com/downloads/downloads_main.html#down_patchmaster) and [Axon Binary (.abf)](https://swharden.com/pyabf/abf2-file-format/) files are supported.
#' @param experiment Index of the experiment to import. Only required if \var{filename} is a PatchMaster file. The experiment is the 1st order hierarchy in PatchMaster file tree. Default is \code{1}.
#' @param series Index of the series to import Only required if \var{filename} is a PatchMaster file. The series is the 2nd order hierarchy in PatchMaster file tree. Default is \code{1}.
#' @param traces Index or indices of Traces/channels to import. The trace is the 3rd order hierarchy in PatchMaster file tree. Must be vector of numeric of length > 0. Default is \code{c(1,2)}.
#' @param encoding File encoding to use, default is \code{getOption("encoding")}
#' @param filetype The type of the file to be imported. Accepted values are 'PatchMaster' or 'ABF' If left blank (default), it will be guessed from the file extension.
#' @param verbose Whether to display verbose messages during the import process. Default is \code{FALSE}.
#' @seealso \linkS4class{PRecording}
#' @examples
#' \dontrun{
#' # import a PatchMaster file
#' tmp<-ImportPRecording("test.dat",series = 1,traces = c(1,2))
#' }
#' @return A \linkS4class{PRecording} object
#' @importFrom tools file_ext
#' @importFrom readABF readABF
#' @export
ImportPRecording <- function(filename,
                             experiment = 1,
                             series = 1,
                             traces = c(1, 2),
                             encoding = getOption("encoding"),
                             filetype = NULL,
                             verbose = FALSE) {
  message(paste("Importing", filename))

  if (!file.exists(filename)) {
    stop("File ", filename, " does not exist")
  }

  if (is.null(filetype)) {
    if (tools::file_ext(filename) == "dat") {
      filetype <- "PatchMaster"
    }
    if (tools::file_ext(filename) == "abf") {
      filetype <- "ABF"
    }
  }

  if (filetype == "PatchMaster") {
    first = T
    for (i in traces) {
      suppressWarnings(
        imp <-
          getSeries(
            filename,
            file = 1,
            exp = experiment,
            ser = series,
            trace = i,
            encoding = encoding
          )
      )
      if (first) {
        first = F
        params <- PRecordingParams(
          Traces = imp$sweeps$tracename,
          RecMode = imp$sweeps$RecMode,
          ProtocolName = imp$sweeps$Stimulus,
          RPip = imp$sweeps$RPip,
          RSeal = imp$sweeps$RSeal,
          URest = imp$sweeps$Urest,
          Cs = imp$sweeps$Cs,
          Rs = imp$sweeps$Rs,
          Experiment = imp$sweeps$exp,
          Series = imp$sweeps$ser,
          Created = as.POSIXct(imp$sweeps$time),
          Filename = imp$sweeps$filename,
          Type = filetype,
          Version = imp$sweeps$version,
          DataScaler = imp$sweeps$DataScaler,
          Unit_from_file = imp$sweeps$Unit_from_file,
          TrYOffset = imp$sweeps$TrYOffset,
          TrYRange = imp$sweeps$TrYRange
        )

        Data <- list()
        Data[[imp$sweeps$tracename]] <- as.matrix(imp$sweeps$y)

        out <- PRecording(
          Traces = imp$sweeps$tracename,
          Units = imp$sweeps$YUnit,
          TimeTrace = imp$sweeps$x[, 1],
          TimeUnit = imp$sweeps$XUnit,
          Sweeps = ordered(colnames(imp$sweeps$y), levels =
                             colnames(imp$sweeps$y)),
          SweepTimes = as.numeric(as.vector(imp$sweeps$Trace_Time)),
          Data = Data,
          Plots = list(),
          RecordingParams = params
        )
        
        if (verbose){
          report <- data.frame(
            Variable = c(
              "Filename",
              "Created",
              "RecMode",
              "ProtocolName",
              "Traces",
              "Sweeps"
            ),
            Value = c(
              imp$sweeps$filename,
              as.POSIXct(imp$sweeps$time),
              imp$sweeps$RecMode,
              imp$sweeps$Stimulus,
              imp$sweeps$tracename,
              length(colnames(imp$sweeps$y))
            )
          )
          print(kable(head(report)))
        }
        
      } else{
        out <- AddTrace(
          X = out,
          Trace = imp$sweeps$tracename,
          Unit = imp$sweeps$YUnit,
          mtx = imp$sweeps$y,
          isOrig = T
        )
      }
      if (verbose) message(paste("Importing trace", i))
    }
  }
  if (filetype == "ABF") {
    tmp <- readABF(filename)

    if (tmp$header$nOperationMode != 5) {
      warning("ImportPRecording only tested for episodic stimulation protocols so far.")
    }

    Data <- list()
    dat <- simplify2array(tmp$data)
    SWEEPnames <- paste0("s", (1:dim(dat)[3]))
    for (i in traces) {
      # dimension 3 is channels/traces in abf file
      Data[[i]] <- dat[, i, ]
      colnames(Data[[i]]) <- SWEEPnames
    }

    names(Data) <- make.names(tmp$channelNames)

    #Experiment type: 0 = Voltage Clamp; 1 = Current Clamp.

    params <- PRecordingParams(
      Traces = make.names(tmp$channelNames),
      RecMode = if (tmp$sections$ProtocolSec$nExperimentType == 0) {
        "Voltage Clamp"
      } else{
        "Current Clamp"
      },
      ProtocolName = "ABFFile",
      RPip = as.numeric(0),
      RSeal = as.numeric(0),
      URest = as.numeric(0),
      Cs = tmp$sections$ADCsec[[1]]$fTelegraphMembraneCap,
      Rs = tmp$sections$ADCsec[[1]]$fTelegraphAccessResistance,
      Experiment = as.character(experiment),
      Series = as.character(series),
      Created = as.POSIXct(paste0(tmp$header$uFileStartDate, "0000"), format =
                             "%Y%m%d%H%M") + tmp$header$uFileStartTimeMS / 1000,
      Filename =  tmp$path,
      Type = tmp$header$fFileSignature,
      Version = tmp$formatVersion
    )

    out <- PRecording(
      Traces = make.names(tmp$channelNames),
      Units = tmp$channelUnits,
      TimeTrace = seq(
        from = 0,
        by = tmp$samplingIntervalInSec,
        length.out = tmp$header$dataPtsPerChan
      ),
      TimeUnit = "s",
      Sweeps = ordered(SWEEPnames, levels =
                         SWEEPnames),
      SweepTimes = tmp$header$sweepStartInPts * tmp$samplingIntervalInSec,
      Data = Data,
      Plots = list(),
      RecordingParams = params
    )
  }

  # RPip, Rseal, Cs and Rs must be greater 0

  return(out)
}
