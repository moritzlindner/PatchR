#' Imports a collection of electrophysiology recordings
#'
#' `r lifecycle::badge("stable")` \cr
#' Imports a collection of electrophysiology recordings. All files in one condition must be acquired using the same recording protocol. I.e. the number of traces/channels, data points per sweep and number of sweeps must be identical.
#'
#' @inheritParams ImportPRecording
#' @param filelist A data.frame. If importing from [Patch Master .dat](https://www.heka.com/downloads/downloads_main.html#down_patchmaster) files, it must contain the following three columns: "FileName","Experiment","Series". FileName can be an absolute path or relative to \var{parent.dir}. "Experiment","Series" correspond to the second and third order hierarchy in the PatchMaster file tree. If importing from [Axon Binary (.abf)](https://swharden.com/pyabf/abf2-file-format/) files, a single column "FileName" is sufficient. An optional column is "Group" and can be used to assign the individual recordings into groups
#' @param parent.dir Parent directory where to search for FileNames from \code{filelist}. Optional if no full path is provided in \var{filelist}.
#' @param filetype The type of the files to be imported. Accepted values are 'PatchMaster' or 'ABF' If left blank (default), it will be guessed from the extension of the first file in list
#' @seealso \linkS4class{PRecording}, \link[=ImportPRecording]{ImportPRecording}
#' @examples
#' \dontrun{
#' rec.df<-data.frame("/path/to/file.dat",1,1)
#' colnames(rec.df)<-c(("FileName","Experiment","Series"))
#' tmp<-ImportCollection(rec.df,traces = c(1,2))
#' }
#' @return A \linkS4class{PCollection} object
#' @export ImportCollection
ImportCollection <- function(filelist = NULL,
                             traces = c(1, 2),
                             parent.dir = "",
                             filetype = NULL,
                             encoding = getOption("encoding")) {

  if (!("Group" %in% colnames(filelist))) {
    filelist$Group <- "Ungrouped"

  }

  print(filelist)
  if (is.null(filetype)){
    if (("FileName" %in% colnames(filelist))) {
      if (tools::file_ext(filelist$FileName[1]) == "dat") {
        filetype <- "PatchMaster"
      }
      if (tools::file_ext(filelist$FileName[1]) == "abf") {
        filetype <- "ABF"
      }
    }else{
      stop(
        "'filelist' provided not in correct format. Must have at least the following three columns: 'FileName', 'Experiment' and 'Series'"
      )
    }
  }

  if (filetype=="PatchMaster"){
    if (!(c("FileName", "Experiment", "Series") %in% colnames(filelist))) {
      stop(
        "'filelist' provided not in correct format. Must have at least the following three columns: 'FileName', 'Experiment' and 'Series'"
      )
    }

    parent.dir <- sub("/$", "", parent.dir)
    filelist$FileName <- paste0(parent.dir, "/", filelist$FileName)
    Recordings <- apply(filelist,
                        1,
                        function(x) {
                          ImportPRecording(filename = x["FileName"],
                                           experiment = as.numeric(x["Experiment"]),
                                           series = as.numeric(x["Series"]),
                                           traces = traces,
                                           encoding = encoding,
                                           filetype = filetype)
                        })
  }

  if (filetype=="ABF"){

    parent.dir <- sub("/$", "", parent.dir)
    filelist$FileName <- paste0(parent.dir, "/", filelist$FileName)
    Recordings <- apply(filelist,
                        1,
                        function(x) {
                          ImportPRecording(filename = x["FileName"],
                                           experiment = 1,
                                           series = 1,
                                           traces = traces,
                                           encoding = encoding,
                                           filetype = filetype)
                        })
  }

  NewPCollection(Recordings, Group = filelist$Group)
}
