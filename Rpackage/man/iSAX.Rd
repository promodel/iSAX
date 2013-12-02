\name{iSAX}
\alias{iSAX}
\title{iSAX}
\description{Converts time series into iSAX representation and return object of class 'iSAX'}
\usage{iSAX(ts, card = 4, wl = 16, win = length(ts), base)}
\arguments{
  \item{ts}{time series to be analyzed}
  \item{card}{alphabet cardinality, number of bits time series will be coded with}
  \item{wl}{number of symbols. After conversion sliding window of the time series will be wl in length.}
  \item{win}{window size. Time series will be converted into set of (l-win+1) strings}
  \item{base}{SAXbase object (optional to increase speed of serial computations)}
}



\author{Anatoly Sorokin}





