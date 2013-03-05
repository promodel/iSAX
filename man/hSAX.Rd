\name{hSAX}
\alias{hSAX}
\title{hSAX}
\description{convert signal into hSAX string  }
\usage{hSAX(ts, wl = 16, win = length(ts), base)}
\arguments{
  \item{ts}{signal}
  \item{wl}{desired length of the string representation}
  \item{win}{sliding window length. Signal will be represented as set of length(ts)-win+1 strings of wl characters each.}
  \item{base}{SAXbase object to speeed up calculations.}
}



\author{Anatoly Sorokin}





