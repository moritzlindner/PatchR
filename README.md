# PatchR
### PatchR: An environment for handling and analyzing electrophysiological (Patch Clamp) measurements

## Installation
```{r}
if (!requireNamespace("remotes", quietly = TRUE))
    install.packages("remotes")
remotes::install_github("moritzlindner/PatchMasteR")
```

## Description
Core component of PatchR is the PRecording class, wich stores a series imported from a Patchmaster *.dat file. Several integrety checks are implemented into the PRecording class to assure data integrity is maintained.
CMultiple recordings (PRecording) objects can be stored and processed within the PCollection class using mostly identical commands and synthax.

Import procedures are currently implemented for HEKA's PachtMaster file format.

## Example
```{r}
library(PatchR)

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
