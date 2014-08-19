\name{dataframe2zoo}
\alias{dataframe2zoo}
\title{Transfroms a data.frame containing \code{year,month,day,hour} into a "zoo" object}
\usage{
dataframe2zoo(x, station = NULL, tz = "A", ...)
}
\arguments{
  \item{x}{data frame}

  \item{station}{names of data.frame columns to be
  considered}

  \item{tz}{timezone. Default is \code{"A"}.}

  \item{...}{further arguments}
}
\value{
a 'zoo' object
}
\description{
Transfroms a data.frame containing
\code{year,month,day,hour} into a "zoo" object
}

