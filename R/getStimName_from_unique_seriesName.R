#' getStimName_from_unique_seriesName
#'
#' function inherited from ephys2 (/github/tdanker/ephys2/). Undocumented.
#' @import stringr
getStimName_from_unique_seriesName<-function (string)
{
  return(stringr::str_split_fixed(string, " : ", 2)[2])
}
