\name{SAX.int}
\alias{SAX.int}
\title{SAX int}
\description{Converts time series into SAX representation and return character string where SAX symbols mapped into integer levels}
\usage{SAX.int(ts, alphasize = 4, wl = 16, win = length(ts))}
\arguments{
  \item{ts}{time series to be analyzed}
  \item{alphasize}{alphabet cardinality, number of levels time series will be split on}
  \item{wl}{number of symbols. After conversion sliding window of the time series will be wl in length.}
  \item{win}{window size. Time series will be converted into set of (l-win+1) strings}
}

\value{integer representation of SAX signal}

\author{Anatoly Sorokin}





