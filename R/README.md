README
================
Moritz Lindner
20 January 2021 

## Installation

``` r
if (!requireNamespace("remotes", quietly = TRUE)){
  install.packages("remotes")
}
remotes::install_github("moritzlindner/PatchMasteR")
```

## Description

Core component of PatchR is the PRecording class, wich stores a series
imported from a Patchmaster \*.dat file. Several integrety checks are
implemented into the PRecording class to assure data integrity is
maintained. Multiple recordings (PRecording) objects can be stored and
processed within the PCollection class using mostly identical commands
and synthax.

Import procedures are currently implemented for HEKA’s PachtMaster file
format.

## Examples

### Example 1: Improting and inspecting data 

Import a PatchMaster \*.dat file, inspect visually and make an average
over all sweeps

``` r
library(PatchR)

# import a PatchMaster file
tmp<-ImportPRecording("test.dat",series = 1,traces = c(1,2))

#alternatively, use the sample data included in the package
data(PRecording)
tmp<-SampleData

# build a Plot superimposing all sweeps and inspect interactivley
tmp<-BuildRecordingPlot(tmp)
Inspect(tmp, what = "I-mon")

# apply any function to the PRecording object. In this case, make mean over all sweeps
tmp<-apply(tmp, "Sweep", mean, ReturnPObject = T)

# and return as data.frame
as.data.frame(tmp)
```

### Example 2: Flexible data analysis

Create a PCollection, perform a classical time series data extraction
(in this case: from each stored PRecording, take current trace “I-mon”,
extract values between 0.8s and 1s from sweep 15 and average these),
and add thesse as metadata.

``` r
exp<-newPCollection(list(SampleData,SampleData),Names=c("A","B"),Group=c("Generic1","Generic2"))

#flexible approach
exp<-AddMetaData(exp, #add as metadata to PCollection
            lapply(exp, # for each PRecording stored in PCollection
                   function(x){
                     apply( # average over time
                       GetData(x, # extract values from I-mon trace, Sweep 15, between 1 and 1.19 s only 
                                  Traces="I-mon",
                                  Time =c(1,1.19),
                                  Sweeps=GetSweepNames(x)[c(3)]),
                       "Time",
                       mean)
                     }
                   )
            )

GetMetaData(exp)
```

*The import functions have been adopted from the [ephys2
package](https://github.com/tdanker/ephys2) from Timm Danker*

Developed by [Moritz
Lindner](lindnerlab.de)
