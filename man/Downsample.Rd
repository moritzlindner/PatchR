% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/downsample.R
\name{Downsample}
\alias{Downsample}
\title{Downsample a PRecording object}
\usage{
Downsample(X, npnts.out = NULL, sample.rate = NULL)
}
\arguments{
\item{X}{A \linkS4class{PRecording} or \linkS4class{PCollection} object}

\item{npnts.out}{Number of points in output time axis, ignored if NULL}

\item{sample.rate}{New sampling rate}
}
\value{
A matrix or \link[=PRecording]{PRecording} object
}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#stable}{\figure{lifecycle-stable.svg}{options: alt='[Stable]'}}}{\strong{[Stable]}} \cr
Downsample \link[=PRecording]{PRecording} object along the time axis
}
