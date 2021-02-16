#' Imports a collection of PatchClamp recordings
#'
#' Imports a collection of PatchClamp recordings. I e those that be long to a the same experimental series.
#'
#' @param parent.dir parent directory where to search for FileNames from filelist
#' @param filelist a data.frame with four columns: "FileName","Experiment","Series","Group". FileName can be absolute or relative to parent.dir. "Experiment","Series" as defined in PatchMaster file. Groups can be assigned via the Group variable
#' @param traces which traces to import. default is \code{c(1,2)}
#' @param type File Type. Currently not implemented. Only works with PatchMatster files.
#' @return A \link[=PMCollection]{PMCollection} object
#' @exportMethod dropPMRecording
ImportCollection<-function(parent.dir=NULL,
                 filelist=NULL,
                 traces=c(1, 2),
                 type="PatchMaster"){
  if(!colnames(filelist)==c("FileName","Experiment","Series","Group")){
    stop("filelist provided not in correct format. Must have three columns, named 'FileName', 'Experiment', 'Series','Group'")
  }

  if(!is.null(parent.dir)){
    filelist$FileName<-paste0(filelist$FileName)
  }
  Recordings<-apply(filelist,1,function(x){ImportPMRecording(x["FileName"],x["Experiment"],x["Series"],traces)})
  newPMCollection(Recordings,filelist$Group)
}
