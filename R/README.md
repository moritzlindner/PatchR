README
================
Moritz Lindner
20 January 2021

## Installation

``` r
remotes::install_github("moritzlindner/PatchMasteR")
```

## Description

Core component of PatchMasteR is the PMTrace class, wich stores a series
imported from a Patchmaster \*.dat file. Several integrety checks are
implemented into the PMTrace class to assure data integrity is
maintained.

## Example

``` r
library(PatchMasteR)

# import a PatchMaster file
tmp<-ImportPMSeries("test.dat",series = 1,traces = c(1,2))

# build a Plot superimposing all sweeps and inspect interactivley
tmp<-BuildTimeSeriesPlot(tmp)
tmp<-InspectTimeSeries(tmp, Trace = "I.mon")

# apply any function to the PMSeries object, in this case, make mean over all sweeps
tmp<-apply(tmp, "Sweep", mean, ReturnPMTRace = T)

# and return as data.fram
as.data.frame(tmp)
```

*The import functions have been adopted from the [ephys2
package](https://github.com/tdanker/ephys2) from Timm Danker*

Developed by [Moritz
Lindner](https://www.uni-marburg.de/en/fb20/departments/physiology/research/dominik-oliver-lab/research2/retinal-physiology-and-gene-therapy)
