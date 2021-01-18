#' readAny
#'
#' function inherited from ephys2 (/github/tdanker/ephys2/). Undocumented.
readAny<-function (ptr, con, offset, what, size)
{
  seek(con, where = ptr + offset)
  an<-readBin(con, what, size = size)
  return(an)
}
