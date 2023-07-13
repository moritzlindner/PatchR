#' 
#' Fit a Boltzmann function to data
#' 
#' #' `r lifecycle::badge("experimental")` \cr
#' The `Boltzmann` method fits a Boltzmann function to data stored in a \linkS4class{PRecording} object. Therefore, values from the stimulus (usually voltage) and response (usually current) traces are averaged between the indicated time windows on a per-sweep basis. Internally, the `boltzmann_fit` function is called to perform the actual fitting. 
#'
#' @param X \linkS4class{PRecording} object
#' @inheritParams GetData
#' @inheritParams MeasureSweeps
#' @param Time_Range_Stim A numeric vector specifying the time range for the stimulus trace .
#' @param Time_Range_Tail_Start A numeric vector specifying the time range for measuring the tail current in the response trace.
#' @param Time_Range_Tail_End A numeric vector specifying the time range for measuring the leak current in the response trace (usually end of the voltage step eliciting the tail currents).
#' @param precision An integer specifying the precision for rounding the stimulus values.
#' @return For `Boltzmann()`: A \linkS4class{PRecording} or \linkS4class{PCollection}, respectively. In \linkS4class{PRecording} the \code{Fit} Slot will be added a list item termed \code{Boltzmann} containing the modeled fit and the \code{MetaData} Slot will contain additional columns as described below. In \linkS4class{PCollection} objects, a \code{V_half}  column will be added to the  \code{MetaData} Slot. If \code{ReturnPObject=F} a \link[base:data.frame]{data.frame} will be returned, as described below.
#' \describe{
#'   \item{V}{The voltage values from the input data.}
#'   \item{G}{The conductance values from the input data.}
#'   \item{G.norm}{The normalized conductance values calculated using the fitted Boltzmann function.}
#'   \item{G.predicted}{The predicted conductance values from the fitted Boltzmann function at the V values.}
#'   \item{G.predicted.norm}{The normalized predicted conductance values from the fitted Boltzmann function at the V values.}
#' }
#' The \link[base:data.frame]{data.frame} also includes the following attributes:
#' \describe{
#'   \item{Vhalf}{The estimated Vhalf parameter of the Boltzmann function.}
#'   \item{k}{The estimated k parameter of the Boltzmann function.}
#'   \item{G_max}{The estimated G_max parameter of the Boltzmann function.}
#' }
#'
#' @examples
#' data("PRecording")
#' df<-Boltzmann(
#'   SampleData,
#'   StimTrace = "V-mon",
#'   RespTrace = "I-mon",
#'   Time_Range_Stim = c(1.18, 1.19),
#'   Time_Range_Tail_Start = c(1.203, 1.204),
#'   Time_Range_Tail_End = c(1.33, 1.38),
#'   precision = 3
#' )
#' df
#' attributes(df)
#' ######
#'                 
#' @importFrom dplyr between
#' @importFrom stats nls
#' @importFrom stats predict
#' @importFrom stats nls.control
#'
#' @name Boltzmann
#' @exportMethod Boltzmann
setGeneric("Boltzmann", function(X,
                                 StimTrace = "V-mon",
                                 RespTrace = "I-mon",
                                 Time_Range_Stim = c(1.18, 1.19),
                                 Time_Range_Tail_Start = c(1.203, 1.204),
                                 Time_Range_Tail_End = c(1.33, 1.38),
                                 precision = 3,
                                 ReturnPObject = T,
                                 ...) {
  standardGeneric("Boltzmann")
})

#' @noMd
setMethod("Boltzmann", "PRecording", function(X,
                                              StimTrace = "V-mon",
                                              RespTrace = "I-mon",
                                              Time_Range_Stim = c(1.18, 1.19),
                                              Time_Range_Tail_Start = c(1.203, 1.204),
                                              Time_Range_Tail_End = c(1.33, 1.38),
                                              precision = 3,
                                              ReturnPObject = T,
                                              ...) {
  range(GetTimeTrace(X))
  if(!all(dplyr::between(Time_Range_Stim,min(GetTimeTrace(X)),max(GetTimeTrace(X))))){
    stop("Values of Time_Range_Stim not within range of time Trace in X")
  }
  
  if(!all(dplyr::between(Time_Range_Tail_Start,min(GetTimeTrace(X)),max(GetTimeTrace(X))))){
    stop("Values of Time_Range_Tail_Start not within range of time Trace in X")
  }  
  
  if(!all(dplyr::between(Time_Range_Tail_End,min(GetTimeTrace(X)),max(GetTimeTrace(X))))){
    stop("Values of Time_Range_Tail_End not within range of time Trace in X")
  }
  
  # get into uniform dimensions
  # get data and bring to uniform range (norm to max)
  X <- MeasureSweeps(
    X,
    Trace = StimTrace,
    label = "Stimulus",
    Time = Time_Range_Stim,
    FUN = mean
  )
  X <- MeasureSweeps(
    X,
    Trace = RespTrace,
    label = "Tail_end",
    Time = Time_Range_Tail_End,
    FUN = mean
  )
  X <- MeasureSweeps(
    X,
    Trace = RespTrace,
    label = "Tail_start",
    Time = Time_Range_Tail_Start,
    FUN = mean
  )
  
  df <- GetMetaData(X)
  G <- df$Tail_end - df$Tail_start
  G_max_rec<-max(G)
  G <- G/G_max_rec
  V <- round(df$Stimulus, precision)
  
  if (length(unique(V)) != length(V)) {
    stop(
      paste(
        "Values in StimTrace are not unique at a precison of",
        precision,
        "decimal digits."
      )
    )
  }
  
  # do fit
  fit<-boltzmann_fit(V, G, ...)
  message(GetRecParam(X,"Filename"))
  print(fit)
  # predict up to extremes by expanding range 10x into each direction

  interval <-
    round((range(V)[2] - range(V)[1]) / (length(V) - 1), precision)
  rangetimes10 <-
    seq((min(V) - max(V)) * 10, (max(V) - min(V)) * 10, interval)
  predicted.value <- predict(fit, list(V = rangetimes10))
  # normalize predicted
  norm.predicted.value <- normalize(predicted.value)
  
  #' normalize true values to the predicted at min and max
  min_x <- min(predicted.value)
  max_x <- max(predicted.value)
  
  G.norm <- (G - min_x) / (max_x - min_x)
  
  df.out <- data.frame(
    V = V,
    G = G*G_max_rec,
    G.norm = G.norm,
    G.predicted = predicted.value[round(rangetimes10, precision) %in% V]*G_max_rec,
    G.predicted.norm = norm.predicted.value[round(rangetimes10, precision) %in% V]
  )
  
  if (!ReturnPObject) {
    # return data frame or add to PatchR: original data, predicted values, normalized predicted, normalized true, attr= V1/2 and K
    attributes(df.out)["Vhalf"] <- coef(fit)[1]
    attributes(df.out)["k"] <- coef(fit)[2]
    attributes(df.out)["G_max"] <- coef(fit)[3]*G_max_rec
    return(df.out)
  } else{
    X <- as(X,"PRecording") # to add Fits Slot for old PRecording Objects
    X <- AddMetaData(X,
                     df.out,
                     title = colnames(df.out),
                     Verbose = F)
    X@Fits[["Boltzmann"]]<-fit
    return(X)
  }

})

#' @noMd
setMethod("Boltzmann", "PCollection", function(X,
                                               StimTrace = "V-mon",
                                               RespTrace = "I-mon",
                                               Time_Range_Stim = c(1.18, 1.19),
                                               Time_Range_Tail_Start = c(1.203, 1.204),
                                               Time_Range_Tail_End = c(1.33, 1.38),
                                               precision = 3,
                                               ReturnPObject = T,
                                               ...) {
  
  if (!ReturnPObject) {
    # if no PObject is returned, can clear the relevant MetaData Slots
    X <- ClearMetaData(X, Verbose = F)
    X <- lapply(X, function(x) {
      x <- ClearMetaData(x, Verbose = F)
    }, ReturnPObject = T)
  }
  X <- lapply(X,
              function(x) {
                tryCatch({
                  Boltzmann(
                    x,
                    StimTrace = StimTrace,
                    RespTrace = RespTrace,
                    Time_Range_Stim = Time_Range_Stim,
                    Time_Range_Tail_Start = Time_Range_Tail_Start,
                    Time_Range_Tail_End = Time_Range_Tail_End,
                    precision = precision,
                    ReturnPObject = T
                  )
                },error = function(e) {
                  warning(
                    "An error occurred when processing ",
                    GetRecParam(x, "Filename"),
                    ": ",
                    conditionMessage(e)
                  )
                  return(x)
                })
              },
              ReturnPObject = T)
  V_half <- lapply(X,
                   function(x) {
                     tryCatch({(coef(x@Fits[["Boltzmann"]])[1])},error = function(e){
                       return(NA)
                     })
                   },
                   ReturnPObject = F)
  
  X <- AddMetaData(X, values = V_half, title = "V_half", Verbose = F)
  
  if (!ReturnPObject) {
    df.out <- data.frame(
      Name = character(),
      Group = character(),
      V = double(),
      G = double(),
      G.norm = double(),
      G.predicted = double(),
      G.predicted.norm = double(),
      V_half = double()
    )
    
    # FIXME: This should become part of GetMetaData for PCollection
    for (i in 1:length(X)) {
      Name <- rep(GetRecordingNames(X)[i], length(GetSweepNames(X)))
      Group <-
        rep(GetGroups(X)[i], length(GetSweepNames(X)))
      
      md <-
        GetMetaData(
          GetData(X, Recordings = GetRecordingNames(X)[i], nowarnings = T),
          c("V", "G", "G.norm", "G.predicted", "G.predicted.norm")
        )
      
      v_half <- rep(GetMetaData(X, "V_half")$V_half[i], length(GetSweepNames(X)))
      
      tmp<-cbind(Name,Group,md,v_half)
      df.out<-rbind(df.out,tmp)
    }
    return(df.out)
  } else{
    return(X)
  }


})

#' Estimate the slope of a curve within specified margins
#'
#' This function estimates the slope of a curve represented by two vectors, V and G,
#' within the specified margins. It calculates the difference in V values corresponding
#' to the range of G values within the margins, and divides it by the difference in
#' the margins to estimate the slope.
#'
#'@param V A numeric vector representing the independent variable.
#' @param G A numeric vector representing the dependent variable.
#' @param margins A numeric vector specifying the lower and upper margins for G values.
#'   The default is c(0.2, 0.8).
#'
#' @return The estimated slope of the curve within the specified margins.
#' 
#' @keywords internal
estimate_slope <- function(V, G, margins = c(0.2, 0.8)) {
  if (!is.numeric(V) || !is.numeric(G)) {
    stop("Input arguments V and G must be numeric vectors.")
  }
  
  if (length(V) != length(G)) {
    stop("Input vectors V and G must have the same length.")
  }
  
  Vrange <-
    V[normalize(G) >= margins[1] & normalize(G) <= margins[2]]
  
  
  if (length(Vrange) <= 1) {
    stop(
      paste(
        "Too few data potins within margins ",
        margins,
        ". Broaden margins or provide k manually"
      )
    )
  }
  
  Vdiff <- max(abs(Vrange)) - min(abs(Vrange))
  return(Vdiff / diff(margins))
}

#' Fit a Boltzmann function to voltage and conductance data
#'
#' The `boltzmann_fit` function fits a Boltzmann function to voltage and conductance data (V and G). It is normally called internally from the `Boltzmann` method to perform the actual fitting, but can also be called directly.
#' It estimates the parameters Vhalf, k, and G_max that best fit the data. The Boltzmann function is defined as: \deqn{(G_max - 0) / (1 + exp((Vhalf - V) / k))} and thus assumes that virtually all channels are in the closed state at the most negative voltage values.
#' 
#' @param V A numeric vector representing the voltage values.
#' @param G A numeric vector representing the conductance values.
#' @param start_Vhalf The initial estimate for Vhalf.
#'   Defaults to the V value corresponding to G closest to 0.5 when normalized.
#' @param start_k The initial estimate for k.
#'   Defaults to the estimated slope of the data within a specified range using the `estimate_slope` function.
#' @param start_G_max The initial estimate for G_max.
#'   Defaults to 1.1 times the maximum G value.
#' @param lower_Vhalf The lower bound for Vhalf.
#'   Defaults to the minimum V value.
#' @param lower_G_max The lower bound for G_max.
#'   Defaults to 0.75 times the mean of G values in the last quarter of the data.
#' @param upper_Vhalf The upper bound for Vhalf.
#'   Defaults to the maximum V value.
#'
#' @return For `boltzmann_fit()`: An object of class "nls" representing the fitted Boltzmann function.
#' @examples
#' V <- c(1, 2, 3, 4, 5)
#' G <- c(0.1, 0.3, 0.5, 0.7, 0.9)
#' fit <- boltzmann_fit(V, G)
#'
#'
#' @importFrom stats nls
#' @importFrom stats nls.control
#'
#' @describeIn Boltzmann Fit a Boltzmann function to voltage and conductance data.
#'
#'
#' @export
boltzmann_fit <-
  function(V,
           G,
           start_Vhlaf = V[which.min(abs(normalize(G) - 0.5))],
           start_k = estimate_slope(V, G),
           start_G_max = max(G) * 1.1,
           lower_Vhalf = min(V),
           lower_G_max = mean(G[seq(length(G) - round(length(G) / 4), length(G))]) * 0.75,
           upper_Vhalf = max(V)
  ){
    
    # validate input parameters
    
    if (!is.numeric(V) || !is.numeric(G)) {
      stop("Input arguments V and G must be numeric vectors.")
    }
    
    if (length(V) != length(G)) {
      stop("Input vectors V and G must have the same length.")
    }
    
    # sub-funcitons
    
    boltzmann_eqn <- function(V, Vhalf, k, G_max) {
      (G_max - 0) / (1 + exp((Vhalf - V) / k))
    }
    
    
    # fit
    
    fit <- tryCatch({
      nls(
        G ~ boltzmann_eqn(V, Vhalf, k, G_max),
        start = list(
          Vhalf = start_Vhlaf,
          k = start_k,
          G_max = start_G_max
        ),
        #lower = c(lower_Vhalf,-Inf, lower_G_max),
        #upper = c(upper_Vhalf, Inf, Inf),
        #algorithm = "port",
        control = list(
          maxiter = 50,
          tol = 1e-6,
          minFactor = 1/8192
        )
      )
    },
    error = function(e) {
      stop("An error occurred during the fitting process: ",
           conditionMessage(e))
    })
    
    
    return(fit)
    
  }
