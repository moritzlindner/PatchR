ImportCollection<-function(parent.dir=NULL,
                 filelist=NULL,
                 traces=c(1, 2)){
  if(!colnames(filelist)==c("FileName","Experiment","Series")){
    stop("filelist provided not in correct format. Must have three columns, named 'FileName', 'Experiment', 'Series'")
  }

  if(!is.null(parent.dir)){
    filelist$FileName<-paste0(filelist$FileName)
  }
  Recordings<-apply(filelist,1,function(x){ImportPMRecording(x$FileName,x$Experiment,x$Series,x$traces)})
  newPMCollection(Recordings)
}
