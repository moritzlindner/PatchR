#' (OK) Plotting methods
#'
#' `r lifecycle::badge("stable")` \cr
#' These methods create "typical" ephsy graphs like dose-response curves, time-series or point statistics.
#'
#' @inheritParams MeasureStimResp
#' @param Sweep Sweep to analyse for group comparison
#' @param fun Function to apply on graph for stimulus response plotting
#' @return A \linkS4class{PCollection} with an item added to the Plots slot if \code{ReturnPMobject=T} or a \link[ggplot2:ggplot]{ggplot}.
#' @examples
#' \dontrun{
#' # return ggplot
#' BuildTimeSeriesPlot(aPRecording, RespTrace = "I-mon", Time = c(0.8,1), fun = mean, ReturnPMobject = F)
#' # return PRecording
#' aPRecording<-BuildTimeSeriesPlot(aPRecording, RespTrace = "I-mon", Time = c(0.8,1), fun = mean, ReturnPMobject = T)
#' Inspect(aPRecording,"TimeSeriesPlot")
#' }
#' @name Plot
NULL

#' @describeIn Plot This method builds a dose-response curve
#' @exportMethod BuildStimRespPlot
setGeneric(
  name = "BuildStimRespPlot",
  def = function(X,
                 StimTrace = "V-mon",
                 RespTrace = "I-mon",
                 Time,
                 fun = mean,
                 ReturnPMobject = T)
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
                   ReturnPMobject = T) {
            BuildStimRespPlotgeneric(X,
                                     StimTrace,
                                     RespTrace,
                                     Time,
                                     fun,
                                     ReturnPMobject)
          })

#' @noRd
setMethod("BuildStimRespPlot",
          "PCollection",
          function(X,
                   StimTrace = "V-mon",
                   RespTrace = "I-mon",
                   Time,
                   fun = mean,
                   ReturnPMobject = T) {
            BuildStimRespPlotgeneric(X,
                                     StimTrace,
                                     RespTrace,
                                     Time,
                                     fun,
                                     ReturnPMobject)
          })


#' @noRd
BuildStimRespPlotgeneric <- function(X,
                                     StimTrace = "V-mon",
                                     RespTrace = "I-mon",
                                     Time,
                                     fun = mean,
                                     ReturnPMobject = T) {
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
      paste0(ConvenientScalessi(dat$Stimulus), X@Series[[1]]@Units[GetTraceNames(X@Series[[1]]) ==
                                                                     StimTrace])
    RespUnit <-
      paste0(ConvenientScalessi(dat$Response), X@Series[[1]]@Units[GetTraceNames(X@Series[[1]]) ==
                                                                     RespTrace])
  }
  dat$Stimulus <- ConvenientScalesvalue(dat$Stimulus)
  dat$Response <- ConvenientScalesvalue(dat$Response)

  if (!("Group" %in% colnames(dat))) {
    dat$Group <- "Genereic"
  }
  out <-
    ggplot2::ggplot(dat,
                    ggplot2::aes_string(y = "Response", x = "Stimulus", colour = "Group")) +
    ggplot2::stat_summary(fun = mean, geom = "line") +
    ggplot2::stat_summary(fun = mean, geom = "point") +
    ggplot2::stat_summary(fun.data = mean_se, geom = "errorbar") +
    ggplot2::theme_classic() +
    ggplot2::theme(
      legend.position = "bottom",
      text = element_text(size = 8),
      strip.background = ggplot2::element_rect(
        fill = "light grey",
        colour = NULL,
        size = 0
      )
    ) +
    ggplot2::xlab(paste(StimTrace, " [", StimUnit, "]")) +
    ggplot2::ylab(paste(RespTrace, " [", RespUnit, "]"))

  if (length(unique(dat$group)) == 1) {
    out <- out + ggplot2::theme(legend.position = "none")
  }

  if (ReturnPMobject) {
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
                 ReturnPMobject = T)
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
                   ReturnPMobject = T) {
            BuildTimeSeriesPlotgeneric(X,
                                       RespTrace,
                                       Time,
                                       fun,
                                       ReturnPMobject)
          })
#' @noRd
setMethod("BuildTimeSeriesPlot",
          "PCollection",
          function(X,
                   RespTrace = "I-mon",
                   Time,
                   fun = mean,
                   ReturnPMobject = T) {
            BuildTimeSeriesPlotgeneric(
              X = X,
              RespTrace = RespTrace,
              Time = Time,
              fun = fun,
              ReturnPMobject = ReturnPMobject
            )
          })
#' @noRd
BuildTimeSeriesPlotgeneric <- function(X,
                                       RespTrace,
                                       Time,
                                       fun,
                                       ReturnPMobject) {
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
      paste0(ConvenientScalessi(dat$StimTimes), X@Series[[1]]@TimeUnit)
    RespUnit <-
      paste0(ConvenientScalessi(dat$Response), X@Series[[1]]@Units[GetTraceNames(X@Series[[1]]) ==
                                                                     RespTrace])

  }
  dat$StimTimes <- ConvenientScalesvalue(dat$StimTimes)
  dat$Response <- ConvenientScalesvalue(dat$Response)

  if (!("Group" %in% colnames(dat))) {
    dat$Group <- "Genereic"
  }
  out <-
    ggplot2::ggplot(dat,
                    ggplot2::aes_string(y = "Response", x = "StimTimes", colour = "Group")) +
    ggplot2::stat_summary(fun = mean, geom = "line") +
    ggplot2::stat_summary(fun = mean, geom = "point") +
    ggplot2::stat_summary(fun.data = mean_se, geom = "errorbar") +
    ggplot2::theme_classic() +
    ggplot2::theme(
      legend.position = "bottom",
      text = element_text(size = 8),
      strip.background = ggplot2::element_rect(
        fill = "light grey",
        colour = NULL,
        size = 0
      )
    ) +
    ggplot2::xlab(paste("Time [", TimeUnit, "]")) +
    ggplot2::ylab(paste(RespTrace, " [", RespUnit, "]"))

  if (length(unique(dat$group)) == 1) {
    out <- out + ggplot2::theme(legend.position = "none")
  }

  if (ReturnPMobject) {
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
                 ReturnPMobject = T)
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
                   ReturnPMobject = T) {
            BuildGroupComparisonPlotgeneric(X,
                                            Sweep,
                                            RespTrace,
                                            Time,
                                            fun,
                                            ReturnPMobject)
          })
#' @noRd
#' @importFrom ggpubr stat_compare_means desc_statby compare_means
BuildGroupComparisonPlotgeneric <- function(X,
                                            Sweep,
                                            RespTrace = "I-mon",
                                            Time,
                                            fun = mean,
                                            ReturnPMobject) {
  X.tmp <- GetData(X, Sweeps = Sweep)
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
      paste0(ConvenientScalessi(dat$Response), X.tmp@Series[[1]]@Units[GetTraceNames(X.tmp@Series[[1]]) ==
                                                                         RespTrace])
  }
  dat$Response <- ConvenientScalesvalue(dat$Response)

  if (!("Group" %in% colnames(dat))) {
    dat$Group <- "Genereic"
  }

  message("Summary statistics")
  cat(desc_statby(dat, "Response", "Group"))
  message("Group comparison")
  cat(compare_means(Response ~ Group, dat))

  out <-
    ggplot(dat, ggplot2::aes_string(y = "Response", x = "Group")) +
    ggplot2::geom_boxplot() +
    ggplot2::geom_point(position = "jitter") +
    ggpubr::stat_compare_means() +
    ggplot2::theme_classic() +
    ggplot2::theme(
      legend.position = "bottom",
      text = element_text(size = 8),
      strip.background = ggplot2::element_rect(
        fill = "light grey",
        colour = NULL,
        size = 0
      )
    ) +
    ggplot2::ylab(paste(RespTrace, " [", RespUnit, "]"))
  if (ReturnPMobject) {
    X@Plots[["GroupComparisonPlot"]] <- out
    X
  } else{
    out
  }
}
