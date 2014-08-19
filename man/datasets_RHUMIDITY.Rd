\name{datasets_RHUMIDITY}
\docType{data}

\alias{list}
\alias{gen_2001_2002_00001}
\alias{gen_2001_2002_00002}
\alias{gen_2001_2002_00003}
\alias{gen_2001_2002_00004}
\alias{gen_2001_2002_00005}
\alias{gen_2001_2002_00006}
\alias{gen_2001_2002_00007}
\alias{gen_2001_2002_00008}
\alias{gen_2001_2002_00009}
\alias{gen_2001_2002_00010}

\title{
Dataset for RHUMIDITY
}

\usage{
data(gen_2001_2002_00001)
data(gen_2001_2002_00002)
data(gen_2001_2002_00010)
}

\description{
It contains the following variables: 
\describe{
\item{\code{list}}{ (for each of \code{gen\_2001\_2002\_000XX}) list of Data frame containing \code{year},\code{month},  \code{day} each and daily minimum temperature (\code{Tn\_gen}), maximum temperature (\code{Tn\_gen}) and precipitation (\code{prec\_gen}) in several points Trentino region. }
%\cr
%\item{\code{pine}}{\code{zoo} object containing hourly time-series of temparature[Celsius Degrees], relative humidity [Percent], precipitation [mm], leaf wetness (actually incomplete) at the Baselga di Pine (Trentino, Italy)  }                 
}
}
\details{
 These data sets contain RMAWGEN generations. See \code{{trentino}} in RMAWGEN packages for point metadata information.
}
\format{
list of Data frames 
}
\source{
RMAWGEN (\url{http://cran.r-project.org/web/packages/RMAWGEN/index.html}) generations from Original data are provided by Provincia Autonoma di Trento (\url{http://www.meteotrentino.it/}), 
Agro-Meteorological Office Technological Transfer CEnter, Fondazione Edmund Mach \url{http://meteo.fmach.it/meteo/}
This dataset is intended for research purposes only, being distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY.
}

%%\references{
%% ~put references to the literature/web site here ~
%%}
\keyword{dataset}
