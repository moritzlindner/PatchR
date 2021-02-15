#' S4 class storing Recording parameters for impoted [Patch Master](https://www.heka.com/downloads/downloads_main.html#down_patchmaster)  Traces.
#'
#' This class stores imported [Patch Master](https://www.heka.com/downloads/downloads_main.html#down_patchmaster)  Traces. Currently only tested for time series. It is strictly validated to assure data consistency

PMRecordingParams<-setClass(Class="PMRecordingParams",
                            slots= list(
                              Traces="character",
                              RecMode="character",
                              ProtocolName="character",
                              RPip="numeric",
                              RSeal="numeric",
                              Urest="numeric",
                              Cs="numeric",
                              Rs="numeric",
                              Experiment="character",
                              Series="character",
                              Created="POSIXct",
                              Filename="character"
                            ),
                            prototype = list(
                              "Created" = Sys.time()
                            )
)
