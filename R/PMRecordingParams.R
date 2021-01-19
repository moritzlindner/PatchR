#' S4 class storing Recording parameters for impoted PatchMaster Traces.
#'
#' This class stores imported PatchMaster Traces. Currently only checked for time series. It is strictly validated to assure data consistency

PMRecordingParams<-setClass(Class="PMRecordingParams",
                            slots= list(
                              Traces="character",
                              RecMode="character",
                              ProtocolName="character",
                              RPip="double",
                              RSeal="double",
                              Urest="double",
                              Cs="double",
                              Rs="double",
                              Experiment="character",
                              Series="character",
                              Created="POSIXct",
                              Filename="character"
                            ),
                            prototype = list(
                              "Created" = Sys.time()
                            )
)
