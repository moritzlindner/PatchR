% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/AddMetaData.R
\name{AddMetaData}
\alias{AddMetaData}
\title{Adds metadata to a PRecording or PCollection object}
\usage{
AddMetaData(X, values, title = colnames(values), Verbose = T)
}
\arguments{
\item{X}{A \linkS4class{PRecording} or \linkS4class{PCollection} object}

\item{values}{The values to be added. Can be anything convertible into a \var{matrix}.}

\item{title}{The title(s) for the metadata column(s).}

\item{Verbose}{should the names/titles of the columns added be printed?}
}
\value{
A \linkS4class{PRecording} or \linkS4class{PCollection} object, respectively.
}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#stable}{\figure{lifecycle-stable.svg}{options: alt='[Stable]'}}}{\strong{[Stable]}} \cr
This function adds metadata to a \linkS4class{PRecording} or \linkS4class{PCollection} object by filling the \var{MetaData} slot. The function calling \code{AddMetaData} is stored in the \var{.MetaDataFx} slot.
}
\details{
\strong{The MetaData slot} \cr
In a \var{PRecording} object the \var{MetaData} slot is a \var{matrix} with each row corresponding to a sweep. \cr
In a \var{PCollection} object the \var{MetaData} slot is a \var{matrix} with each row corresponding to a \var{PRecording} stored in the object.
Column names must be unique.
}
\examples{
data("PRecording")
# add two columns of metadata
SampleData<-AddMetaData(SampleData,1:length(GetSweepNames(SampleData)),"ID1")
SampleData<-AddMetaData(SampleData,length(GetSweepNames(SampleData)):1,"ID2")
GetMetaData(SampleData,"ID1")
GetMetaData(SampleData)
}
\seealso{
\linkS4class{PRecording}, \linkS4class{PCollection}, \href{../../PatchR/html/apply-PRecording-method.html}{apply()}, \href{../../PatchR/html/lapply-PCollection-method.html}{lapply()}, \link[base:as.matrix]{as.matrix()}
}
