\name{air_humidity}
\alias{air_humidity}
\title{It calcultes air umidity from hourly observation of temperature and daily time series of dew point temperature}
\usage{
air_humidity(T_hourly, Td, percent = TRUE)
}
\arguments{
  \item{T_hourly}{hourly air temperature time-series}

  \item{Td}{daily air dew point temperature}

  \item{percent}{logical value. If \code{TRUE} (default)
  the relative humidity is returned in per cent}
}
\value{
hourly time series of relative humidity (\code{zoo} object)
}
\description{
It calcultes air umidity from hourly observation of
temperature and daily time series of dew point temperature
}
\note{
TBoth the variables \code{T_hourly} and \code{Td} must be
\code{zoo} object referred to the same period
}
\examples{
rm(list=ls())

library(RHUMIDITY)
library(zoo)
data(pine) # contains pine hourly datasat
index(pine) <- as.POSIXlt(index(pine),tz="A") # changes index class type



## get daily temperature Time series

station <- c("PINE")

# Calibratipn coefficien for polygonal relatioships between DDI and Daily Daw Point Temperature by Emanuele Eccel

intercept_prec <- c(1.2)
names(intercept_prec) <- station

coeff_prec <- c(-0.37)
names(coeff_prec) <- station


intercept_noprec <- c(2.7)
names(intercept_noprec) <- station

coeff_noprec <- c(-1.5)
names(coeff_noprec) <- station #adjusted!!

T_hourly <- pine$T
prec_hourly <- pine$prec
RH_hourly <- pine$RH


## daily aggregation

prec <- aggregate(prec_hourly,by=as.Date(index(T_hourly)),FUN=sum)
Tm <- aggregate(T_hourly,by=as.Date(index(T_hourly)),FUN=mean)
Tn <- aggregate(T_hourly,by=as.Date(index(T_hourly)),FUN=min)
Tx<- aggregate(T_hourly,by=as.Date(index(T_hourly)),FUN=max)

### Daily dew temperature

Td <- TDEW(Tx=Tx,Tn=Tn,Tmean=Tm,prec=prec,lag=10,valmin_prec=0.5,intercept_prec=intercept_prec,intercept_noprec=intercept_noprec,coeff_prec=coeff_prec,coeff_noprec=coeff_noprec,DDI_lim=NULL)



RH_hourly_calc <- air_humidity(T_hourly=T_hourly,Td=Td)

# Date from which the measurement instrument work well!!!
data0 <- as.POSIXlt("2011-04-01 00:00:00",tz="A")

# start and end dates for a possible visualization!!!

start <- as.POSIXlt("2012-05-01 00:00:00",tz="A")
end <- as.POSIXlt("2012-05-15 23:00:00",tz="A")

days_calc <- (index(RH_hourly_calc)>=start & index(RH_hourly_calc)<=end)
days <- (index(RH_hourly)>=start & index(RH_hourly)<=end)

day_correct <-  (index(RH_hourly)>=data0)

RH_hourly_calc_m <- RH_hourly_calc[days_calc]
RH_hourly_m <- RH_hourly[days]
T_hourly_m <- T_hourly[days]
prec_hourly_m <- prec_hourly[days]

# temporary plot

plot(RH_hourly_m)
lines(RH_hourly_calc_m,col=2)

# daily aggregation of relative humidity
#
RHm <- aggregate(RH_hourly,by=as.Date(index(T_hourly)),FUN=mean)
RHn <- aggregate(RH_hourly,by=as.Date(index(T_hourly)),FUN=min)
RHx<- aggregate(RH_hourly,by=as.Date(index(T_hourly)),FUN=max)

RHm_calc <- aggregate(RH_hourly_calc,by=as.Date(index(T_hourly)),FUN=mean)
RHn_calc <- aggregate(RH_hourly_calc,by=as.Date(index(T_hourly)),FUN=min)
RHx_calc <- aggregate(RH_hourly_calc,by=as.Date(index(T_hourly)),FUN=max)
}
\seealso{
\code{\link{TDEW}}
}

