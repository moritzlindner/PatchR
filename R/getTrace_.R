#' getTrace_
#'
#' function inherited from ephys2 (/github/tdanker/ephys2/). Undocumented.
#' @noRd
getTrace_<-function (con, ptr, start = 0, n = NA, read_data = T, name = "",
                     con_dat = con)
{
  SIZE = 2
  tracename <- name
  ptr <- attr(ptr, "dataptr")
  seek(con, ptr + 40)
  offset <- readBin(con, "int", size = 4)
  nDatapoints_ <- readBin(con, "int", size = 4)
  nDatapoints <- nDatapoints_ - start
  if (!is.na(n)) {
    nDatapoints <- min(nDatapoints, n)
  }
  seek(con, ptr + 96)
  
  
  Unit = readBin(con, "char")
  if (Unit == "V")
    Unit_from_file = 1000
  else Unit_from_file = 1e+09
  
  Unit_=1
  seek(con, ptr + 72)
  DataScaler = readBin(con, "double", size = 8)
  seek(con_dat, where = offset + start * SIZE)
  if (read_data) {
    trace <- readBin(con_dat, what = "int", size = SIZE,
                     n = nDatapoints) * Unit_ * DataScaler
  }
  else {
    trace <- NA
  }
  seek(con, ptr + 104)
  Xinterval <- readBin(con, "double", size = 8)
  attr(trace, "Xinterval") <- Xinterval
  attr(trace, "nDatapoints_") <- nDatapoints_
  attr(trace, "name") <- tracename


  seek(con, ptr + 68)
  attr(trace, "RecMode") <-c( "InOut",
                              "OnCell",
                              "OutOut",
                              "WholeCell",
                              "CClamp",
                              "VClamp",
                              "NoMode" )[readBin(con, "int", size = 1)+1] # recording mode
  seek(con, ptr + 152)
  attr(trace, "RPip")<-(readBin(con, "double", size = 8)) # Rpip
  seek(con, ptr + 160)
  attr(trace, "Urest")<-(readBin(con, "double", size = 8)) # Urest
  seek(con, ptr + 168)
  attr(trace, "RSeal")<-(readBin(con, "double", size = 8)) # RSeal
  seek(con, ptr + 176)
  attr(trace, "Cs")<-(readBin(con, "double", size = 8)) # Cs
  seek(con, ptr + 184)
  attr(trace, "Rs")<-(1/readBin(con, "double", size = 8)) # Rs

  seek(con, ptr + 120)
  attr(trace, "XUnit")<-(readBin(con, "character", size = 8)) # Rs

  seek(con, ptr + 96)
  attr(trace, "YUnit")<-(readBin(con, "character", size = 8)) # Rs

  attr(trace, "DataScaler")<-DataScaler
  attr(trace, "Unit_from_file")<-Unit_from_file    
  
  seek(con, ptr + 136)
  attr(trace, "TrYOffset")<-(readBin(con, "double", size = 8)) # TrYOffset
  seek(con, ptr + 128)
  attr(trace, "TrYRange")<-(readBin(con, "double", size = 8)) # TrYRange
  
     
  trace
}
