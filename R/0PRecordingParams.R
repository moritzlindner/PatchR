# S4 class storing Recording parameters inside \linkS4class{PRecording} and A \linkS4class{PCollection} objects
#
# Usually not required to be manipulated by the user

PRecordingParams<-methods::setClass(Class="PRecordingParams",
                            slots= list(
                              Traces="character",
                              RecMode="character",
                              ProtocolName="character",
                              RPip="numeric",
                              RSeal="numeric",
                              URest="numeric",
                              Cs="numeric",
                              Rs="numeric",
                              Experiment="character",
                              Series="character",
                              Created="POSIXct",
                              Filename="character",
                              Type="character",
                              Version="character"
                            ),
                            prototype = list(
                              "Created" = Sys.time()
                            )
)
