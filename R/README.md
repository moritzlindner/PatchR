README
================
Moritz Lindner
20 January 2021

## Installation

``` r
remotes::install_github("moritzlindner/PatchMasteR")
```

## Description

Core component of PatchMasteR is the PMRecording class, wich stores a series
imported from a Patchmaster \*.dat file. Several integrety checks are
implemented into the PMRecording class to assure data integrity is
maintained.

## Examples

### Example 1:

Import a PatchMaster \*.dat file, inspect visually and make an average
over all sweeps

``` r
library(PatchMasteR)

# import a PatchMaster file
tmp<-ImportPMRecording("test.dat",series = 1,traces = c(1,2))

# build a Plot superimposing all sweeps and inspect interactivley
tmp<-BuildSeriesPlot(tmp)
tmp<-InspectSeries(tmp, Trace = "I.mon")

# apply any function to the PMRecording object, in this case, make mean over all sweeps
tmp<-apply(tmp, "Sweep", mean, ReturnPMRecording = T)

# and return as data.frame
as.data.frame(tmp)
```

### Example 2:

Create a PMCollection, perform a classical time series data extraction
(in this case: from each stored PMRecording, take current trace “I-mon”,
extract values between 1s and 1.19s from sweep 15 and average these),
and add thesse as metadata.

``` r
exp<-newPMCollection(list(tmp,tmp),Names=c("A","B"),Group=c("Generic1","Generic2"))

AddMetaData(exp, #add as metadata to PMCollection
            lapply(exp, # for each PMRecording stored in PMCollection
                   function(x){
                     apply( # average over time
                       SubsetData(x, # extract values from I-mon trace, Sweep 15, between 1 and 1.19 s only 
                                  Traces="I-mon",
                                  Time =c(1,1.19)
                                  Sweeps=SweepNames(x)[c(15)]),
                       "Time",
                       mean)
                     }
                   )
            )

exp@MetaData
```

*The import functions have been adopted from the [ephys2
package](https://github.com/tdanker/ephys2) from Timm Danker*

Developed by [Moritz
Lindner](https://www.uni-marburg.de/en/fb20/departments/physiology/research/dominik-oliver-lab/research2/retinal-physiology-and-gene-therapy)
