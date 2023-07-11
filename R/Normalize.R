#' Normalize a numeric vector to a specified range
#'
#' The `normalize` function takes a numeric vector and normalizes its values
#' to a specified range.
#'
#' @param x A numeric vector to be normalized.
#' @param range A numeric vector specifying the desired range. The default range is [0, 1].
#'
#' @return A numeric vector with values normalized to the specified range.
#'
#' @examples
#' x <- c(1, 2, 3, 4, 5)
#' normalize(x)  # Normalize x to the default range [0, 1]
#' normalize(x, range = c(0, 100))  # Normalize x to the range [0, 100]
#'
#' @seealso
#' \code{\link{scale}}
#'
#' @export
normalize <- function(x, range = c(0, 1)) {
  if (!is.numeric(x)) {
    stop("Input x must be a numeric vector")
  }

  if (!is.numeric(range) | !length(range)==2) {
    stop("Input range must be a numeric vector of length 2")
  }
    
  min_x <- min(x)
  max_x <- max(x)
  
  normalized_x <- (x - min_x) / (max_x - min_x)
  
  min_range <- range[1]
  max_range <- range[2]
  
  normalized_x * (max_range - min_range) + min_range
}


#' FIXME: TURN INTO METHOD FOR both PObjects and document
#' turn into scale function allow for scale "toMax", "MintoMax"

#' @noMd
#' @noRd
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