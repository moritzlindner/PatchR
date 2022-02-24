#' Plotting methods
#'
#' `r lifecycle::badge("stable")` \cr
#' These methods create "typical" electrophysiology graphs, like stimulus-response curves, time-series or point statistics.
#'
#' @inheritParams MeasureStimResp
#' @param Sweep Sweep to analyse for group comparison
#' @param fun Function to apply on graph for stimulus response plotting
#' @return A \linkS4class{PCollection} with an item added to the Plots slot if \code{ReturnPObject=T} or a \link[ggplot2:ggplot]{ggplot}.
#' @seealso  \link[=Inspect]{Inspect}, \link[=Measure]{Measure}, \link[=GetPlot]{GetPlot}, \link[=GetPlotNames]{GetPlotNames}
#' @examples
#' # Build a stimulus response curve from a single recording. Here, a current
#' # response, averaged from 0.8-1s of the trace is used. A PRecording object with
#' # an updated slot Plots is returned.
#' data(PRecording)
#' SampleData<-BuildStimRespPlot(
#'   SampleData,
#'   StimTrace = "V-mon",
#'   RespTrace = "I-mon",
#'   Time = c(0.8,1),
#'   fun = mean,
#'   ReturnPObject = TRUE
#' )
#' # Interactively analyse graph
#' Inspect(SampleData,"StimRespPlot")
#'
#'  # Build a time series. Average is taken from RespTrace between 0.8s and 1s for
#'  # each sweep and is plotted against the timestamp of the sweep. The graph
#'  # (ggplot) is returned directly.
#' BuildTimeSeriesPlot(SampleData,
#'    RespTrace = "I-mon",
#'    Time = c(0.8,1),
#'    fun = mean,
#'    ReturnPObject = FALSE)
#' @name Plot
NULL

#' @describeIn Plot This method builds a stimulus-response curve
#' @exportMethod BuildStimRespPlot
setGeneric(
  name = "BuildStimRespPlot",
  def = function(X,
                 StimTrace = "V-mon",
                 RespTrace = "I-mon",
                 Time,
                 fun = mean,
                 ReturnPObject = T)
  {
    standardGeneric("BuildStimRespPlot")
  }
)

#' @noRd
setMethod("BuildStimRespPlot",
          "PRecording",
          function(X,
                   StimTrace = "V-mon",
                   RespTrace = "I-mon",
                   Time,
                   fun = mean,
                   ReturnPObject = T) {
            BuildStimRespPlotgeneric(X,
                                     StimTrace,
                                     RespTrace,
                                     Time,
                                     fun,
                                     ReturnPObject)
          })

#' @noRd
setMethod("BuildStimRespPlot",
          "PCollection",
          function(X,
                   StimTrace = "V-mon",
                   RespTrace = "I-mon",
                   Time,
                   fun = mean,
                   ReturnPObject = T) {
            BuildStimRespPlotgeneric(X,
                                     StimTrace,
                                     RespTrace,
                                     Time,
                                     fun,
                                     ReturnPObject)
          })


#' @importFrom ggplot2 ggplot geom_line aes_string theme_classic theme element_text xlab ylab stat_summary element_rect
#' @noRd
BuildStimRespPlotgeneric <- function(X,
                                     StimTrace = "V-mon",
                                     RespTrace = "I-mon",
                                     Time,
                                     fun = mean,
                                     ReturnPObject = T) {
  dat <- MeasureStimResp(X,
                         StimTrace,
                         RespTrace,
                         Time,
                         fun)

  if (class(X)[1] == "PRecording") {
    StimUnit <-
      paste0(ConvenientScalessi(dat$Stimulus), X@Units[GetTraceNames(X) == StimTrace])
    RespUnit <-
      paste0(ConvenientScalessi(dat$Response), X@Units[GetTraceNames(X) == RespTrace])
  } else{
    StimUnit <-
      paste0(ConvenientScalessi(dat$Stimulus), X@Recordings[[1]]@Units[GetTraceNames(X@Recordings[[1]]) ==
                                                                         StimTrace])
    RespUnit <-
      paste0(ConvenientScalessi(dat$Response), X@Recordings[[1]]@Units[GetTraceNames(X@Recordings[[1]]) ==
                                                                         RespTrace])
  }
  dat$Stimulus <- ConvenientScalesvalue(dat$Stimulus)
  dat$Response <- ConvenientScalesvalue(dat$Response)

  if (!("Group" %in% colnames(dat))) {
    dat$Group <- "Genereic"
  }
  out <-
    ggplot(dat,
           aes_string(y = "Response", x = "Stimulus", colour = "Group")) +
    stat_summary(fun = mean, geom = "line") +
    stat_summary(fun = mean, geom = "point") +
    stat_summary(fun.data = mean_se, geom = "errorbar") +
    theme_classic() +
    theme(
      legend.position = "bottom",
      text = element_text(size = 8),
      strip.background = element_rect(
        fill = "light grey",
        colour = NULL,
        size = 0
      )
    ) +
    xlab(paste(StimTrace, " [", StimUnit, "]")) +
    ylab(paste(RespTrace, " [", RespUnit, "]"))

  if (length(unique(dat$group)) == 1) {
    out <- out + theme(legend.position = "none")
  }

  if (ReturnPObject == T) {
    X@Plots[["StimRespPlot"]] <- out
    X
  } else{
    out
  }
}


#' @describeIn Plot This method builds a time Series plot
#' @exportMethod BuildTimeSeriesPlot
setGeneric(
  name = "BuildTimeSeriesPlot",
  def = function(X,
                 RespTrace = "I-mon",
                 Time,
                 fun = mean,
                 ReturnPObject = T)
  {
    standardGeneric("BuildTimeSeriesPlot")
  }
)

#' @noRd
setMethod("BuildTimeSeriesPlot",
          "PRecording",
          function(X,
                   RespTrace = "I-mon",
                   Time,
                   fun = mean,
                   ReturnPObject = T) {
            BuildTimeSeriesPlotgeneric(X,
                                       RespTrace,
                                       Time,
                                       fun,
                                       ReturnPObject)
          })
#' @noRd
setMethod("BuildTimeSeriesPlot",
          "PCollection",
          function(X,
                   RespTrace = "I-mon",
                   Time,
                   fun = mean,
                   ReturnPObject = T) {
            BuildTimeSeriesPlotgeneric(
              X = X,
              RespTrace = RespTrace,
              Time = Time,
              fun = fun,
              ReturnPObject = ReturnPObject
            )
          })
#' @importFrom ggplot2 ggplot geom_line aes_string theme_classic theme element_text xlab ylab stat_summary element_rect
#' @noRd
BuildTimeSeriesPlotgeneric <- function(X,
                                       RespTrace,
                                       Time,
                                       fun,
                                       ReturnPObject) {
  dat <- MeasureStimResp(
    X = X,
    StimTrace = RespTrace,
    RespTrace = RespTrace,
    Time = Time,
    FUN = fun
  )
  if (class(X)[1] == "PRecording") {
    TimeUnit <- paste0(ConvenientScalessi(dat$StimTimes), X@TimeUnit)
    RespUnit <-
      paste0(ConvenientScalessi(dat$Response), X@Units[GetTraceNames(X) == RespTrace])
  } else{
    TimeUnit <-
      paste0(ConvenientScalessi(dat$StimTimes),
             X@Recordings[[1]]@TimeUnit)
    RespUnit <-
      paste0(ConvenientScalessi(dat$Response), X@Recordings[[1]]@Units[GetTraceNames(X@Recordings[[1]]) ==
                                                                         RespTrace])

  }
  dat$StimTimes <- ConvenientScalesvalue(dat$StimTimes)
  dat$Response <- ConvenientScalesvalue(dat$Response)

  if (!("Group" %in% colnames(dat))) {
    dat$Group <- "Genereic"
  }
  out <-
    ggplot(dat,
           aes_string(y = "Response", x = "StimTimes", colour = "Group")) +
    stat_summary(fun = mean, geom = "line") +
    stat_summary(fun = mean, geom = "point") +
    stat_summary(fun.data = mean_se, geom = "errorbar") +
    theme_classic() +
    theme(
      legend.position = "bottom",
      text = element_text(size = 8),
      strip.background = element_rect(
        fill = "light grey",
        colour = NULL,
        size = 0
      )
    ) +
    xlab(paste("Time [", TimeUnit, "]")) +
    ylab(paste(RespTrace, " [", RespUnit, "]"))

  if (length(unique(dat$group)) == 1) {
    out <- out + theme(legend.position = "none")
  }

  if (ReturnPObject == T) {
    X@Plots[["TimeSeriesPlot"]] <- out
    X
  } else{
    out
  }
}

#' @describeIn Plot This method builds a boxplot for comparison between groups as stored in the \linkS4class{PCollection}.
#' @exportMethod BuildGroupComparisonPlot
setGeneric(
  name = "BuildGroupComparisonPlot",
  def = function(X,
                 Sweep,
                 RespTrace = "I-mon",
                 Time,
                 fun = mean,
                 ReturnPObject = T)
  {
    standardGeneric("BuildGroupComparisonPlot")
  }
)
#' @noRd
setMethod("BuildGroupComparisonPlot",
          "PCollection",
          function(X,
                   Sweep,
                   RespTrace = "I-mon",
                   Time,
                   fun = mean,
                   ReturnPObject = T) {
            BuildGroupComparisonPlotgeneric(X,
                                            Sweep,
                                            RespTrace,
                                            Time,
                                            fun,
                                            ReturnPObject)
          })
#' @importFrom ggplot2 ggplot geom_line geom_point geom_boxplot aes_string theme_classic theme element_text xlab ylab stat_summary element_rect
#' @importFrom ggpubr stat_compare_means desc_statby compare_means
#' @importFrom knitr kable
#' @noRd
BuildGroupComparisonPlotgeneric <- function(X,
                                            Sweep,
                                            RespTrace = "I-mon",
                                            Time,
                                            fun = mean,
                                            ReturnPObject) {
  X.tmp <- GetData(X, Sweeps = Sweep, nowarnings = T)
  dat <- MeasureStimResp(X.tmp,
                         StimTrace = RespTrace,
                         RespTrace,
                         Time,
                         fun)
  if (class(X.tmp)[1] == "PRecording") {
    RespUnit <-
      paste0(ConvenientScalessi(dat$Response), X.tmp@Units[GetTraceNames(X.tmp) == RespTrace])
  } else{
    RespUnit <-
      paste0(ConvenientScalessi(dat$Response),
             X.tmp@Recordings[[1]]@Units[GetTraceNames(X.tmp@Recordings[[1]]) ==
                                           RespTrace])
  }
  dat$Response <- ConvenientScalesvalue(dat$Response)

  if (!("Group" %in% colnames(dat))) {
    dat$Group <- "Genereic"
  }
  message("Summary statistics")
  kable(desc_statby(dat, "Response", "Group"))
  message("Group comparison")
  kable(compare_means(Response ~ Group, dat))

  out <-
    ggplot(dat, aes_string(y = "Response", x = "Group")) +
    geom_boxplot() +
    geom_point(position = "jitter") +
    stat_compare_means() +
    theme_classic() +
    theme(
      legend.position = "bottom",
      text = element_text(size = 8),
      strip.background = element_rect(
        fill = "light grey",
        colour = NULL,
        size = 0
      )
    ) +
    ylab(paste(RespTrace, " [", RespUnit, "]"))
  if (ReturnPObject == T) {
    X@Plots[["GroupComparisonPlot"]] <- out
    X
  } else{
    out
  }
}
