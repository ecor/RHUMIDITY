\name{aggregate_daily}
\alias{aggregate_daily}
\title{It aggragetes \code{x1} subdaily variable at the daily scale according to \code{x2} aggregation}
\usage{
aggregate_daily(x1, x2, FUN = max)
}
\arguments{
  \item{x1}{subdaily variable to aggregate, e.g.
  temperature}

  \item{x2}{subdaily secondary variable to which \code{FUN}
  is applied for aggregagation of \code{x1}, e.g. relative
  humdity}

  \item{FUN}{function used for aggregation of \code{x1}. It
  is recommened that \code{FUN} be \code{max} or
  \code{min}.}
}
\value{
The daily aggragation requested for \code{x1}, e.g. the
temparature at the time instant in which relative humidity
is maximum.
}
\description{
It aggragetes \code{x1} subdaily variable at the daily
scale according to \code{x2} aggregation
}

