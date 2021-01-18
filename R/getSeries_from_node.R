#' getSeries_from_node
#'
#' function inherited from ephys2 (/github/tdanker/ephys2/). Undocumented.
#' @import stringr
getSeries_from_node<-function (node, sweeps = 1:length(node), trace = 1, read_data = T)
{
  path <- attr(node, "path")
  filename <- path[1]
  if (!file.exists(filename)) {
    filename <- paste("../", filename, sep = "")
  }
  con <- file(filename, "rb")
  signature <- readChar(con, 4)
  if (signature == "DAT1") {
    mypul <- stringr::str_replace(filename, ".dat$",
                                  ".pul")
    if (!file.exists(mypul))
      stop("while trying to read from *.dat file, detected the old 'DAT1' format (no bundle file), therefore expected but could not find a correspoding *.pul file")
    con_pul <- file(mypul, "rb")
  }
  else {
    con_pul <- con
  }
  series <- lapply(node[sweeps], function(sweep) {
    getTrace_(con_pul, sweep[[trace]], read_data = read_data,
              name = names(sweep)[[trace]], con_dat = con)
  })

  SwTimer <- unlist(lapply(node[sweeps], function(sweep) {
    readAny(attr(sweep, "dataptr"), con_pul, 56, "double",
            8)
  }))

  close(con)
  if (signature == "DAT1") {
    close(con_pul)
  }
  maxLength <- max(unlist(lapply(series, length)))
  series <- lapply(series, function(s) {
    length(s) <- maxLength
    s
  })
  series_Y <- do.call(cbind, series)
  Xinterval <- attr(series[[1]], "Xinterval")
  nDatapoints_ <- attr(series[[1]], "nDatapoints_")
  RecMode <- attr(series[[1]], "RecMode")
  RPip <- attr(series[[1]], "RPip")
  Urest <- attr(series[[1]], "Urest")
  RSeal <- attr(series[[1]], "RSeal")
  Cs <- attr(series[[1]], "Cs")
  Rs <- attr(series[[1]], "Rs")

  XUnit <- attr(series[[1]], "XUnit")
  YUnit <- attr(series[[1]], "YUnit")

  series_X1 <- 1:nDatapoints_ * Xinterval
  series_X <- matrix(rep(series_X1, length(series_Y[1, ])),
                     nDatapoints_, length(series_Y[1, ]))
  series <- list(y = series_Y,
                 x = series_X,
                 filename = filename,
                 exp = path[2],
                 ser = path[3],
                 trace = trace,
                 RecMode = RecMode,
                 RPip = RPip,
                 Urest = Urest,
                 RSeal = RSeal,
                 Cs = Cs,
                 Rs = Rs,
                 XUnit = XUnit,
                 YUnit = YUnit,
                 tracename = attr(series[[1]],
                                  "name"),
                 Trace_Time = SwTimer,
                 Serieslabel = getStimName_from_unique_seriesName(path[3]),
                 Stimulus = attr(node, "StimulusName"))
  class(series) <- "HEKAseries"
  series
}
