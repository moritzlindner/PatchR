# PatchMasteR
### An environment for handling and analzing Patch Clamp measuremnts recorded with [HEKA Patchmaster (R)](https://www.heka.com/downloads/downloads_main.html#down_patchmaster) 

## Installation
```{r}
remotes::install_github("moritzlindner/PatchMasteR")
```

## Description

Core component of PatchMasteR is the PMRecording class, wich stores a series imported from a Patchmaster *.dat file. Several integrety checks are implemented into the PMRecording class to assure data integrity is maintained.
Multiple recordings (PMRecording) objects can be stored and processed within the PMCollection class.

## Example
```{r}
library(PatchMasteR)

# import a PatchMaster file
tmp<-ImportPMSeries("test.dat",series = 1,traces = c(1,2))

# build a Plot superimposing all sweeps and inspect interactivley
tmp<-BuildSeriesPlot(tmp)
tmp<-InspectSeries(tmp, Trace = "I.mon")

# apply any function to the PMSeries object, in this case, make mean over all sweeps
tmp<-apply(tmp, "Sweep", mean, ReturnPMTRace = T)

# and return as data.frame
as.data.frame(tmp)

```


*The import functions have been adopted from the [ephys2 package](https://github.com/tdanker/ephys2) from Timm Danker*

Developed by [Moritz Lindner](https://www.uni-marburg.de/en/fb20/departments/physiology/research/dominik-oliver-lab/research2/retinal-physiology-and-gene-therapy)
