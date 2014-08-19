\name{temperature_from_saturated_water_vapor_pressure}
\alias{temperature_from_saturated_water_vapor_pressure}
\title{It is the inverse of \code{\link{temperature_from_saturated_water_vapor_pressure}}}
\usage{
temperature_from_saturated_water_vapor_pressure(es, interval = c(-100, 100),
  root = TRUE, ...)
}
\arguments{
  \item{es}{saturated water vapor pressure in hPa}

  \item{interval}{Temperature interval see
  \code{\link{uniroot}}. Default is \code{c(-100,100)}}

  \item{root}{logical value. If \code{TRUE} (Default) only
  root value is returned, it is used only if \code{es} has
  length 1.}

  \item{...}{arguments for \code{\link{uniroot}}}
}
\description{
It is the inverse of
\code{\link{temperature_from_saturated_water_vapor_pressure}}
}

