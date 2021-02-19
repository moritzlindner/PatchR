#' read.bundletree
#'
#' function inherited from ephys2 (/github/tdanker/ephys2/). Undocumented.
#' @noRd
read.bundletree<-function (myfile, bundlename = ".pul", con = NA,encoding=getOption("encoding"))
{
  finally_close_con = is.na(con)
  message(encoding)
  if (is.na(con)) {
    con <- file(myfile, "rb", encoding=encoding)
  }
  seek(con, 0)
  signature <- readChar(con, 8)
  message(signature)
  if (signature == "DAT2") {
    version <- readChar(con, 32)
    time <- readBin(con, "double")
    nitems <- readBin(con, "int", size = 1)
    liddle_endian <- readBin(con, "logical")
    reserved <- readChar(con, 11)
    bundleitems <- do.call(rbind, (lapply(0:nitems, function(item) {
      start <- readBin(con, "int", size = 4)
      end <- readBin(con, "int", size = 4)
      name <- readChar(con, 8)
      data.frame(start = start, end = end, name = name,
                 stringsAsFactors = F)
    })))
    start <- subset(bundleitems, name == bundlename)$start
    seek(con, where = start)
  }
  else {
    start <- 0
  }
  seek(con, where = start)
  magic <- readChar(con, nchars = 4)
  stopifnot(magic == "eerT")
  nLevels <- readBin(con, "int", size = 4)
  lvl_sizes <- lapply(1:nLevels, function(i) {
    readBin(con, "int", size = 4)
  })
  tree <- pm_load_nodes(con, nLevels, lvl_sizes, 1)
  if (finally_close_con)
    close(con)
  tree
}
