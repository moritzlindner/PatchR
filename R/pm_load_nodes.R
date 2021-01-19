#' pm_load_nodes
#'
#' function inherited from ephys2 (/github/tdanker/ephys2/). Undocumented.
#' @noRd
pm_load_nodes<-function (con, nlevels, lvl_sizes, level)
{
  stopifnot(level <= nlevels)
  size <- lvl_sizes[[level]]
  dataptr <- seek(con)
  seek(con, seek(con) + size)
  nchildren <- readBin(con, "int", size = 4)
  if (nchildren == 0) {
    node <- "trace"
    attr(node, "dataptr") <- dataptr
  }
  else {
    node <- lapply(1:nchildren, function(child) {
      pm_load_nodes(con, nlevels, lvl_sizes, level + 1)
    })
    attr(node, "dataptr") <- dataptr
  }
  node
}
