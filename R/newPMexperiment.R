#' Generates a new PMExperiment from a single PMSeries
#'
#' Generates a new PMExperiment from a single PMSeries
#'
#' @param PMSeries a PMSeries object, or list of PMSeries objects
#' @param
#' @param Group Group variable or list thereof, of the same lenght as PMSeries
#' @return A \link[=PMExperiment]{PMExperiment} object
#' @export
newPMExperiment<-function(PMSeries,
                          Names=NULL,
                          Group="Generic"){
                            Exps<-list()
                            if(!is.list(PMSeries)){
                              Exps[[1]]<-PMSeries
                              if(is.null(Names)){
                                Names<-character()
                                Names<-PMSeries@RecordingParams@Filename
                              }
                              params<-PMRecordingParams(ProtocolName=PMSeries@RecordingParams@ProtocolName,
                                                        RecMode=PMSeries@RecordingParams@RecMode,
                                                        Traces=PMSeries@RecordingParams@Traces
                              )
                            }else{ #list of PMSeries Objects provided
                              Exps<-PMSeries
                              Group<-rep(Group,length(Exps))
                                #Trim Traces if requires
                                # could include function f trimming PMSeries to common minium or orignals, if required. Make dropTrace function therefore.

                              if(is.null(Names)){
                                Names<-character()
                                Names<-unlist(lapply(PMSeries,function(x) x@RecordingParams@Filename))
                              }
                              params<-PMRecordingParams(ProtocolName=PMSeries[[1]]@RecordingParams@ProtocolName,
                                                        RecMode=PMSeries[[1]]@RecordingParams@RecMode,
                                                        Traces=PMSeries[[1]]@RecordingParams@Traces
                              )
                            }
                            out<-PMExperiment(
                              Series=Exps,
                              Names=Names,
                              Group=as.factor(Group),
                              RecordingParams=params
                            )

                          }
