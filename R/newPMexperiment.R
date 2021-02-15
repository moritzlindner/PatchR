#' Generates a new PMCollection from a single PMRecording
#'
#' Generates a new PMCollection from a single PMRecording
#'
#' @param PMRecording a PMRecording object, or list of PMRecording objects
#' @param Names Names of the PMRecording, character vector w same number of entires as PMRecording
#' @param Group Group variable or list thereof, of the same lenght as PMRecording
#' @return A \link[=PMCollection]{PMCollection} object
#' @export
newPMCollection<-function(PMRecording,
                          Names=NULL,
                          Group="Generic"){
                            Exps<-list()
                            if(!is.list(PMRecording)){
                              Exps[[1]]<-PMRecording
                              if(is.null(Names)){
                                Names<-character()
                                Names<-PMRecording@RecordingParams@Filename
                              }
                              params<-PMRecordingParams(ProtocolName=PMRecording@RecordingParams@ProtocolName,
                                                        RecMode=PMRecording@RecordingParams@RecMode,
                                                        Traces=PMRecording@RecordingParams@Traces
                              )
                            }else{ #list of PMRecording Objects provided
                              Exps<-PMRecording
                              if(length(Group)==1){
                                Group<-rep(Group,length(Exps))
                              }
                                #Trim Traces if requires
                                # could include function f trimming PMRecording to common minium or orignals, if required. Make dropTrace function therefore.

                              if(is.null(Names)){
                                Names<-character()
                                Names<-unlist(lapply(PMRecording,function(x) x@RecordingParams@Filename))
                              }
                              params<-PMRecordingParams(ProtocolName=PMRecording[[1]]@RecordingParams@ProtocolName,
                                                        RecMode=PMRecording[[1]]@RecordingParams@RecMode,
                                                        Traces=PMRecording[[1]]@RecordingParams@Traces
                              )
                            }
                            out<-PMCollection(
                              Series=Exps,
                              Names=Names,
                              Group=as.factor(Group),
                              RecordingParams=params
                            )

                          }
