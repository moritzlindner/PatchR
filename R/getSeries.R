#' getSeries
#'
#' function inherited from ephys2 (/github/tdanker/ephys2/). Undocumented.
#' @importFrom proto proto
#' @noRd
getSeries<-function (node_or_tree, file = NA, exp = NA, ser = NA, trace = 1,
                     read_data = T, CompoundName = NA,encoding=getOption("encoding"))
{
  if (is.character(node_or_tree))
    node_or_tree <- get_treeinfo(node_or_tree,encoding=encoding)
  if (!inherits(node_or_tree, "HEKA_treeinfo_seriesnode")) {
    node <- node_or_tree[[c(file, exp, ser)]]
    explabel <- attr(node_or_tree[[c(file, exp)]], "ExperimentLabel")
    if (is.null(explabel))
      explabel <- ""
  }
  else {
    stop
  }
  stopifnot(inherits(node, "HEKA_treeinfo_seriesnode"))
  s <- getSeries_from_node(node, trace = trace, read_data = read_data,encoding=encoding)
  SeriesProto <- proto(sweeps = s)
  if (!is.na(CompoundName))
    SeriesProto$sweeps$CompoundName = CompoundName
  class(SeriesProto) <- c(class(SeriesProto), "HEKAseriesProto")
  SeriesProto
}
