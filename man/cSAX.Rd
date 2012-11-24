\name{cSAX}
\alias{cSAX}
\title{cSAX}
\description{Converts time series into SAX representation and return character string where SAX symbols mapped into letters of Latin alphabet for screen representation  }
\usage{cSAX(ts, alphasize = 4, wl = 16, win = length(ts))}
\arguments{
  \item{ts}{time series to be analyzed}
  \item{alphasize}{alphabet cardinality, number of levels time series will be split on}
  \item{wl}{number of symbols. After conversion sliding window of the time series will be wl in length.}
  \item{win}{window size. Time series will be converted into set of (l-win+1) strings}
}

\value{Latin representation of the signal SAX string}

\author{Anatoly Sorokin}





