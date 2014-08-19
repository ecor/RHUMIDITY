\name{DER}
\alias{DDI}
\alias{DER}
\alias{ET}
\alias{TDEW}
\title{Empirical Potential Evapotranspiration and Daily Drought Index}
\usage{
DER(doy = 1, lat = 46, unit = "MJ_per_square_meter_per_day",
  vaporization_latent_heat = 2.45, divide_by_latent_heat = FALSE)

ET(doy = 1, lat = 46, vaporization_latent_heat = 2.45, Tn = 10,
  Tx = 20, Tmean = 14)

DDI(doy = 1, lat = 46, vaporization_latent_heat = 2.45, Tn = 10,
  Tx = 20, Tmean = 14, prec = 0, lag = 6, DDI_lim = c(0, 0.5, 1, 3),
  ops = "/")

TDEW(doy = 1, lat = 46, vaporization_latent_heat = 2.45, Tn = 10,
  Tx = 20, Tmean = 14, prec = 0, lag = 6, valmin_prec = 0.5,
  intercept_prec, intercept_noprec, coeff_prec, coeff_noprec, DDI_lim = NULL)
}
\arguments{
  \item{doy}{day of the year}

  \item{lat}{latitude expressed in degrees North}

  \item{unit}{output measurement unit}

  \item{vaporization_latent_heat}{water vaporization latent
  heat expressed in MJ per Kg. Default is 2.45.}

  \item{divide_by_latent_heat}{logical values. If
  \code{TRUE} \code{unit} is set equal to
  \code{"Kg_per_square_meter_per_day"}. Default is
  \code{FALSE}.}

  \item{prec}{daily precipitation expressed in mm}

  \item{Tx,Tn,Tmean}{Daily Maximum, Minimun and Mean
  Temperature expressed in Celsius Degrees}

  \item{DDI_lim}{a vector in case \code{DDI} must returns
  numerical catagories or integer number. Default is
  \code{NULL} and \code{DDI} is returned as a continuous
  numerical value}

  \item{ops}{operation used for \code{DDI} computation.
  Default is "/"}

  \item{lag}{number of days used for calculation of DDI.
  See reference.}

  \item{valmin_prec}{precipitation threshold value under
  which no daily precipitation is considered. Default is
  0.5 mm.}

  \item{intercept_prec,intercept_noprec,coeff_prec,coeff_noprec}{values
  for piecewise linear function for dew temperature
  correctio. See FAO method in the Reference.}
}
\description{
Calculates Daily Extraterrestrial radiation (DER) according
to the formules indicated by FAO and Daily Potential
Evapotranspiration (ET) according to Hargerves' formula and
Drought Daily Index (DDI) and Daily Dew Point Temperature
(TDEW)
}
\details{
These functions implement the theory illustrated in the
following paper: from
\url{http://onlinelibrary.wiley.com/doi/10.1002/met.258/abstract}
}
\examples{
# See examples in "air_humidity" function
}
\author{
Emanuele Cordano
}
\references{
See the URLs in DESCRIPTION files
}
\seealso{
\code{\link{air_humidity}}
}

