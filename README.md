# PatchR
### PatchR: An environment for handling and analyzing electrophysiological (Patch Clamp) measurements in R

## Installation
```{r}
if (!requireNamespace("remotes", quietly = TRUE)){
  install.packages("remotes")
}
remotes::install_github("moritzlindner/PatchMasteR")
```

## Description
Core component of PatchR is the PRecording class, which stores a series imported from a Patchmaster *.dat file. Several integrity checks are implemented into the PRecording class to assure data integrity is maintained.
Multiple recordings (PRecording) objects can be stored and processed within the PCollection class using mostly identical commands and syntax.

Import procedures are currently implemented for HEKA's PachtMaster file format.

## Examples

### Example 1:


```{r Example1, eval=FALSE, include=T}
library(PatchR)

# import a PatchMaster file
tmp<-ImportPRecording("test.dat",series = 1,traces = c(1,2))

# build a Plot superimposing all sweeps and inspect interactivley
tmp<-PlotRecording(tmp)
tmp<-InspectSeries(tmp, Trace = "I.mon")

# apply any function to the PRecording object, in this case, make mean over all sweeps
tmp<-apply(tmp, "Sweep", mean, ReturnPMObject = T)

# and return as data.frame
as.data.frame(tmp)
```

### Example 2:
Create a PCollection, perform a classical time series data extraction (in this case: from each stored PRecording, take current trace "I-mon", extract values between 1s and 1.19s from sweep 15 and average these), and add thesse as metadata.
```{r Example2, eval=FALSE, include=T}

exp<-newPCollection(list(tmp,tmp),Names=c("A","B"),Group=c("Generic1","Generic2"))

AddMetaData(exp, #add as metadata to PCollection
            lapply(exp, # for each PRecording stored in PCollection
                   function(x){
                     apply( # average over time
                       getData(x, # extract values from I-mon trace, Sweep 15, between 1 and 1.19 s only 
                                  Traces="I-mon",
                                  Time =c(1,1.19),
                                  Sweeps=getSweepNames(x)[c(15)]),
                       "Time",
                       mean)
                     }
                   )
            )

exp@MetaData

```


*The import functions have been adopted from the [ephys2 package](https://github.com/tdanker/ephys2) from Timm Danker*

Developed by [Moritz Lindner](https://www.uni-marburg.de/en/fb20/departments/physiology/research/dominik-oliver-lab/research2/retinal-physiology-and-gene-therapy)
