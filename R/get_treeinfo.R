#' get_treeinfo
#'
#' function inherited from ephys2 (/github/tdanker/ephys2/). Undocumented.
#' @noRd
get_treeinfo<-function (filename, slim = F)
{
  con = file(filename, "rb")
  signature <- readChar(con, 4)
  if (!(signature == "DAT1" || signature == "DAT2"))
    stop("file type is not supported")
  if (signature == "DAT1") {
    mypul <- stringr::str_replace(filename, ".dat$",
                                  ".pul")
    if (!file.exists(mypul)) {
      close(con)
      stop("while trying to read from *.dat file, detected the old 'DAT1' format (no bundle file), therefore expected but could not find a correspoding *.pul file")
    }
    con_pul <- file(mypul, "rb")
    mypgf <- stringr::str_replace(filename, ".dat$",
                                  ".pgf")
    if (!file.exists(mypul)) {
      close(con)
      stop("while trying to read from *.dat file, detected the old 'DAT1' format (no bundle file), therefore expected but could not find a correspoding *.pgf file")
    }
    con_pgf <- file(mypgf, "rb")
  }
  else {
    con_pul = con
    con_pgf = con
  }
  treeinfo <- list(root = read.bundletree(filename, ".pul",
                                          con = con_pul))
  attr(treeinfo[["root"]], "filename") <- filename
  names(treeinfo) <- filename
  class(treeinfo) <- "HEKA_treeinfo"
  treeinfo <- add_atributes(treeinfo, con_pul, verbose = !slim,
                            filename = filename, con_pgf = con_pgf)
  if (signature == "DAT1") {
    close(con_pul)
    close(con_pgf)
  }
  close(con)
  treeinfo
}
