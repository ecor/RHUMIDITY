\name{saturated_water_vapor_pressure}
\alias{saturated_water_vapor_pressure}
\title{It calculates the saturated water vapor pressure head (expressed in hPa) in function of air temperature}
\usage{
saturated_water_vapor_pressure(T, root = 0)
}
\arguments{
  \item{T}{temperature expressed in Celsius degrees}

  \item{root}{trial value of saturated water vapor
  pressure. It is 0 (Default) and is only used by
  \code{\link{temperature_from_saturated_water_vapor_pressure}}}
}
\value{
The water vapor pressure expressed in hPa
}
\description{
It calculates the saturated water vapor pressure head
(expressed in hPa) in function of air temperature
}
\references{
Eccel at al, 2012
}

