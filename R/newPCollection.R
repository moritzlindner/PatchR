#' Generates a new PCollection from a single PRecording
#'
#' Generates a new \linkS4class{PCollection} from a single \linkS4class{PRecording}
#'
#' @param PRecording a PRecording object, or list of PRecording objects
#' @param Names Names of the PRecording, character vector w same number of entires as PRecording
#' @param Group Group variable or list thereof, of the same lenght as PRecording
#' @return A \linkS4class{PCollection}object
#' @export
newPCollection<-function(PRecording,
                          Names=NULL,
                          Group="Generic"){
                            Exps<-list()
                            if(!is.list(PRecording)){
                              Exps[[1]]<-PRecording
                              if(is.null(Names)){
                                Names<-character()
                                Names<-PRecording@RecordingParams@Filename
                              }
                              params<-PRecordingParams(ProtocolName=PRecording@RecordingParams@ProtocolName,
                                                        RecMode=PRecording@RecordingParams@RecMode,
                                                        Traces=PRecording@RecordingParams@Traces
                              )
                            }else{ #list of PRecording Objects provided
                              Exps<-PRecording
                              if(length(Group)==1){
                                Group<-rep(Group,length(Exps))
                              }
                                #Trim Traces if requires
                                # could include function f trimming PRecording to common minium or orignals, if required. Make dropTrace function therefore.

                              if(is.null(Names)){
                                Names<-character()
                                Names<-as.character(unlist(lapply(PRecording,function(x) x@RecordingParams@Filename)))
                              }
                              params<-PRecordingParams(ProtocolName=PRecording[[1]]@RecordingParams@ProtocolName,
                                                        RecMode=PRecording[[1]]@RecordingParams@RecMode,
                                                        Traces=PRecording[[1]]@RecordingParams@Traces
                              )
                            }
                            out<-PCollection(
                              Series=Exps,
                              Names=Names,
                              Group=as.factor(Group),
                              RecordingParams=params
                            )

                          }
