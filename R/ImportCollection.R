#' (UPD PCLAMP) Imports a collection of PatchClamp recordings
#'
#' Imports a collection of PatchClamp recordings. All files in one condition must be acquired using the same recording protocol. i.e. number of traces, data points per sweep, number of sweeps, etc must be identical.
#'
#' @param parent.dir parent directory where to search for FileNames from \code{filelist}
#' @param filelist a data.frame with four columns: "FileName","Experiment","Series","Group". FileName can be absolute or relative to parent.dir. "Experiment","Series" as defined in PatchMaster file. Groups can be assigned via the Group variable
#' @param traces which traces to import. default is \code{c(1,2)}
#' @param type File Type. Currently not implemented. Only works with PatchMatster files.
#' @param encoding file encoding to use, default is \code{getOption("encoding")}
#' @return A \link[=PCollection]{PCollection} object
#' @export ImportCollection
ImportCollection<-function(parent.dir=getwd(),
                 filelist=NULL,
                 traces=c(1, 2),
                 type="PatchMaster",
                 encoding=getOption("encoding")){
  if(!colnames(filelist)==c("FileName","Experiment","Series","Group")){
    stop("filelist provided not in correct format. Must have three columns, named 'FileName', 'Experiment', 'Series','Group'")
  }
  parent.dir<-sub("/$","",parent.dir)
  filelist$FileName<-paste0(parent.dir,"/",filelist$FileName)
  Recordings<-apply(filelist,
                    1,
                    function(x){ImportPRecording(x["FileName"],
                                                 as.numeric(x["Experiment"]),
                                                 as.numeric(x["Series"]),
                                                 traces,
                                                 encoding=encoding)})
  newPCollection(Recordings,Group = filelist$Group)
}
#
