#' FIXME: TURN INTO METHOD FOR both PObjects and document
#' turn into scale function allow for scale "toMax", "MintoMax"

Normalize_PCollection <- function(object, Trace) {
  
  # split ananlig to CurrentDensity

  # take into account normalization is done per recording

  lapply(object, function(X) {

    object <-

      AddTrace(

        X,

        mtx = X@Data[[Trace]] / max(max(X@Data[[Trace]])),

        Trace = "Norm",

        Unit = paste0(X@Units[GetTraceNames(X) == Trace], "/F"),

        Sweeps = GetSweepNames(X)

      )

    object

  }, ReturnPObject = T)
  
}