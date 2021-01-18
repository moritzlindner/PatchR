#' add_atributes
#'
#' function inherited from ephys2 (/github/tdanker/ephys2/). Undocumented.
add_atributes<-function (tree, con, verbose = F, filename, con_pgf = con)
{
  bundletree.pgf <- read.bundletree(filename, ".pgf",
                                    con_pgf)
  for (rootname in names(tree)) {
    attr(tree[[c(rootname)]], "class") <- c("HEKA_treeinfo_rootnode",
                                            "HEKA_treeinfo")
    GRLabels <- lapply(tree[[c(rootname)]], function(item) {
      readlabel(attr(item, "dataptr"), con)
    })
    attr(tree[[c(rootname)]], "names") <- paste(1:length(tree[[c(rootname)]]),
                                                ":", GRLabels)
    for (exp in names(tree[[rootname]])) {
      attr(tree[[c(rootname, exp)]], "class") <- c("HEKA_treeinfo_experimentnode",
                                                   "HEKA_treeinfo")
      sernames <- lapply(tree[[c(rootname, exp)]], function(item) {
        readlabel(attr(item, "dataptr"), con)
      })
      attr(tree[[c(rootname, exp)]], "names") <- paste(1:length(tree[[c(rootname,
                                                                        exp)]]), ":", sernames)
      label <- readlabel(attr(tree[[c(rootname, exp)]],
                              "dataptr"), con)
      expnumber <- readAny(attr(tree[[c(rootname, exp)]],
                                "dataptr"), con, 116, "int", 4)
      if (!label == paste0("E-", expnumber)) {
        attr(tree[[c(rootname, exp)]], "ExperimentLabel") <- label
      }
      for (ser in names(tree[[c(rootname, exp)]])) {
        attr(tree[[c(rootname, exp, ser)]], "class") <- c("HEKA_treeinfo_seriesnode",
                                                          "HEKA_treeinfo")
        attr(tree[[c(rootname, exp, ser)]], "path") <- c(rootname,
                                                         exp, ser)
        sweeplabels = lapply(tree[[c(rootname, exp, ser)]],
                             function(item) {
                               readlabel(attr(item, "dataptr"), con)
                             })
        attr(tree[[c(rootname, exp, ser)]], "names") <- paste("s",
                                                              1:length(tree[[c(rootname, exp, ser)]]), ":",
                                                              sweeplabels, sep = "")
        stimcount <- readAny(attr(tree[[c(rootname, exp,
                                          ser)]][[1]], "dataptr"), con, 40, "int",
                             2)
        StimName <- readAny(attr(bundletree.pgf[[stimcount]],
                                 "dataptr"), con_pgf, 4, "char",
                            NA)
        attr(tree[[c(rootname, exp, ser)]], "StimulusName") <- StimName
        label = readlabel(attr(tree[[c(rootname, exp,
                                       ser)]], "dataptr"), con)
        if (!label == StimName) {
          attr(tree[[c(rootname, exp, ser)]], "SeriesLabel") <- label
        }
        if (verbose) {
          for (sweep in names(tree[[c(rootname, exp,
                                      ser)]])) {
            attr(tree[[c(rootname, exp, ser, sweep)]],
                 "path") <- c(rootname, exp, ser,
                              sweep)
            attr(tree[[c(rootname, exp, ser, sweep)]],
                 "class") <- c("HEKA_treeinfo_sweepnode",
                               "HEKA_treeinfo")
            attr(tree[[c(rootname, exp, ser, sweep)]],
                 "names") <- lapply(tree[[c(rootname,
                                            exp, ser, sweep)]], function(trace) {
                                              readlabel(attr(trace, "dataptr"),
                                                        con)
                                            })
            for (trace in names(tree[[c(rootname, exp,
                                        ser, sweep)]])) {
              attr(tree[[c(rootname, exp, ser, sweep,
                           trace)]], "path") <- c(rootname,
                                                  exp, ser, sweep, trace)
              attr(tree[[c(rootname, exp, ser, sweep,
                           trace)]], "class") <- c("HEKA_treeinfo_tracenode",
                                                   "HEKA_treeinfo")
            }
          }
        }
      }
    }
  }
  tree
}
