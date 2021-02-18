#' getStimName_from_unique_seriesName
#'
#' function inherited from ephys2 (/github/tdanker/ephys2/). Undocumented.
#' @importFrom stringr str_split_fixed
#' @noRd
getStimName_from_unique_seriesName<-function (string)
{
  return(str_split_fixed(string, " : ", 2)[2])
}
