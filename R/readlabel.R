#' readlabel
#'
#' function inherited from ephys2 (/github/tdanker/ephys2/). Undocumented.
readlabel<-function (ptr, con, offset = 4)
{
  seek(con, where = ptr + offset)
  lab<-readBin(con, "char")
  return(lab)
}
