#' Generates a new PCollection from PRecording(s)
#'
#' `r lifecycle::badge("stable")` \cr
#' Generates a new \linkS4class{PCollection} from one single or a list of \linkS4class{PRecording}(s)
#'
#' @param PRecording a \linkS4class{PRecording} object, or list of PRecording objects
#' @param Names Name(s) of the \linkS4class{PCollection}. A character vector with the same number of entries as \var{PRecording}
#' @param Group Group assignments for the \linkS4class{PRecording}s. A character vector length 1 (all same group) or of the same length as \var{PRecording}
#' @examples
#' data(PRecording)
#' SampleData
#' tmp<-NewPCollection(SampleData)
#' tmp
#' @return A \linkS4class{PCollection}object
#' @export
NewPCollection <- function(PRecording,
                           Names = NULL,
                           Group = "Ungrouped") {
  Exps <- list()
  if (!is.list(PRecording)) {
    Exps[[1]] <- PRecording
    if (is.null(Names)) {
      Names <- character()
      Names <- PRecording@RecordingParams@Filename
    }
    params <-
      PRecordingParams(
        ProtocolName = PRecording@RecordingParams@ProtocolName,
        RecMode = PRecording@RecordingParams@RecMode,
        Traces = PRecording@RecordingParams@Traces
      )
  } else{
    #list of PRecording objects provided
    Exps <- PRecording
    if (length(Group) == 1) {
      Group <- rep(Group, length(Exps))
    }else{
      if(!(length(Group)) == length(Exps)){
        stop("Number of groups given deos not match number of Recordings")
      }
    }
    #Trim Traces if requires
    # could include function f trimming PRecording to common minium or orignals, if required. Make dropTrace function therefore.

    if (is.null(Names)) {
      Names <- character()
      Names <-
        as.character(unlist(lapply(Exps, function(x) {
          GetRecParam(x, "Filename")
        })))
    }
    params <-
      PRecordingParams(
        ProtocolName = Exps[[1]]@RecordingParams@ProtocolName,
        RecMode = Exps[[1]]@RecordingParams@RecMode,
        Traces = Exps[[1]]@RecordingParams@Traces
      )
  }
  out <- PCollection(
    Recordings = Exps,
    Names = Names,
    Group = as.factor(Group),
    RecordingParams = params
  )

}

#' @noMd
newPCollection <- NewPCollection
